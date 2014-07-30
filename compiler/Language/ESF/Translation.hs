{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ESF.Translation (transESF) where

import Language.ESF.Syntax
import Language.SystemF.Syntax

import Data.Maybe       (fromMaybe, fromJust)
import Data.List        (intercalate)
import qualified Data.Map as Map

transESF :: Expr -> PFExp t e
transESF = transExpr Map.empty (Map.empty, Map.empty) . dsLet Map.empty

-- type Mapping a = Map.Map Name a

transTyp :: Mapping t -> Typ -> PFTyp t
transTyp tmap = go
  where
    go Int               = FInt
    go (Fun t1 t2)       = FFun (go t1) (go t2)
    go (Product ts)      = FProduct (map go ts)
    go (TVar a)          =
      case Map.lookup a tmap of
        Just a' -> FTVar a'
        Nothing -> error ("transTyp: Lookup failed for type variable " ++ a)
    go (Forall []     t) = go t
    go (Forall (a:as) t) = FForall (\a' -> transTyp (Map.insert a a' tmap) (Forall as t))

transExpr :: Env -> (Mapping t, Mapping e) -> Expr -> PFExp t e
transExpr env (tmap, emap) = go
  where
    go (Var x)            =
      case Map.lookup x emap of
        Just e  -> FVar x e
        Nothing -> error ("transExpr: Lookup failed for variable " ++ x)
    go (Lit (Integer i))  = FLit i
    go (App e1 e2)        = FApp (go e1) (go e2)
    go (PrimOp op e1 e2)  = FPrimOp (go e1) op (go e2)
    go (If0 p i e)        = FIf0 (go p) (go i) (go e)
    go (Tuple es)         = FTuple (map go es)
    go (Proj expr i)      = FProj i (go expr)
    go (BLam [] e)        = go e
    go (BLam (a:as) e)    = FBLam (\a' -> transExpr env (Map.insert a a' tmap, emap) (BLam as e))
    go (Lam [] e)         = go e
    go (Lam ((x,t):as) e) = FLam (transTyp tmap t) (\x' -> transExpr env (tmap, Map.insert x x' emap) (Lam as e))
    go (TApp e t)         = FTApp (go e) (transTyp tmap t)

    --     let rec f (x : t1) : t2 = def in body
    -- ~~> let f = fix (f : t1 -> t2). \x. def in body
    -- ~~> (\(f : t1 -> t2). body) (fix (f : t1 -> t2). \x. def)
    go (Let Rec [lbind] body) = FApp lam fix
      where
        (f, Lam [(x, t1)] def, Fun _should_equal_t1 t2) = dsLocalBind Rec env lbind
        lam = go (Lam [(f, Fun t1 t2)] body)
        fix = FFix (\f' x' -> transExpr env (tmap, (Map.insert x x' . Map.insert f f') emap) def) (transTyp tmap t1) (transTyp tmap t2)

    go (Let Rec [] _)      = error "transESF: Invariant failed: Let Rec [] _"
    go (Let Rec (_:_:_) _) = error "transESF: Invariant failed: Let Rec (_:_:_) _"
    go (Let NonRec _ _)    = error "transESF: Invariant failed: Let NonRec _ _"

-- Eliminate all non-recursive let expressions, and rewrite mutually recursive
-- let expressions into non-mutually recursive ones
dsLet :: Env -> Expr -> Expr
dsLet = go
  where
    --     let f1 = e1, f2 = e2, f3 = e3 in e
    -- ~~> (\(f1 : infer e1). (\(f2 : infer e2). (\(f3 : infer e3). e) e3) e2) e1
    go env (Let NonRec [] body)             = body
    go env (Let NonRec (lbind:lbinds) body) = App (Lam [(f1, t1)] inside) e1
      where (f1, e1, t1) = dsLocalBind NonRec env lbind
            inside       = go env (Let NonRec lbinds body)

    --     let rec f1 = e1, f2 = e2, f3 = e3 in e
    -- ~~> let rec f (dummy : Int) : (...) = (e1, e2, e3) in e
    -- ~~> let rec f (dummy : Int) : (...) =
    --     [ f1 -> (f 0)._0, f2 -> (f 0)._1, f3 = (f 0)._2 ] inside (e1, e2, e3) and e
    go env (Let Rec [] body)      = body
    go env (Let Rec [lbind] body) = Let Rec [lbind] body
    go env (Let Rec lbinds body)  = Let Rec [lbind'] (Let NonRec [] body)
      where
        (fs, es, ts) = unzip3 (map (dsLocalBind Rec env) lbinds)
        mergedName   = intercalate "_" fs -- TODO Make sure the id is fresh
        lbind' = LocalBind { local_id     = mergedName
                           , local_targs  = []
                           , local_args   = [("dummy", Int)]    -- TODO Make sure the id is free
                           , local_rettyp = Just (Product ts)
                           , local_rhs    = Tuple es
                           }
    go _ expr = expr

-- Take a localbind and return a triple of the name, value, and type:
--
--     f A1 ... An (x1 : T1) ... (xn : Tn) : T = e
-- ~~> ( f                                             -- name
--     , /\A1. ... /\An. \(x1 : T1). ... \(xn : Tn). e -- def
--     , forall A1 ... An. T1 -> ... -> Tn -> T        -- typ
--     )
dsLocalBind :: RecFlag -> Env -> LocalBind -> (Name, Expr, Typ)
dsLocalBind recFlag env LocalBind{..} =
  let name = local_id
      def  = (wrap BLam local_targs . wrap Lam local_args ) local_rhs
      inferred_type = fromJust $ infer env def
  in
  case local_rettyp of
    Nothing ->
      case recFlag of
        Rec    -> error "Return type missing in let rec"
        NonRec -> (name, def, inferred_type)
    Just rettyp ->
      let annoted_type = wrap Forall local_targs $ joinTyps ([t | (_, t) <- local_args] ++ [rettyp]) in
      case recFlag of
        Rec    -> (name, def, inferred_type)
        NonRec ->
          if annoted_type == inferred_type
            then (name, def, annoted_type)
            else error "Type mismatch"

wellformed :: Map.Map Name Type -> Type -> Bool
wellformed d (TVar x)      = x `elem` d
wellformed d  Int          = True
wellformed d (Forall a t') = wellformed d t'
wellformed d (Fun t1 t2)   = wellformed t1 && wellformed t2
wellformed d (Product ts)  = wellformed d `all` ts

infer :: Map.Map Name Typ -> Expr -> Maybe Typ
infer g d = go
  where
    go (Var x) = Map.lookup x vars

    go (Lit (Integer _)) = return Int

    go (BLam as body) = do
      t_body <- go body
      return $ Forall as t_body

    go (Lam [] body)           = go body
    go (Lam ((x,t):args) body) = do
      t_body <- infer (tvars, Map.insert x t vars) (Lam args body)
      return $ Fun t t_body

    {-
      τ ok in Δ   Δ;Γ ⊢ e : ∀α. τ'
      ----------------------------
          Δ;Γ ⊢ e[τ] : τ'[τ/α]
    -}
    go (TApp e t)
      | wellformed d t =
        case go e of
          Forall [] t'     -> Nothing
          Forall (a:as) t' -> substFreeTVars (a, t) (Forall as t)
          _                -> Nothing
      | otherwise = Nothing

    go (App f arg) = do
      t_f <- go f
      case t_f of
        Fun a b -> do
          t_arg <- go arg
          if t_arg == a then return b else Nothing
        _       -> Nothing

    go (PrimOp _ e1 e2) = do
      t1 <- go e1
      t2 <- go e2
      if t1 == Int && t2 == Int
        then Int
        else Nothing

    go (Tuple exprs) = do
      t_exprs <- mapM go exprs
      return $ Product t_exprs

    go (Proj e i) = do
      t_e <- go e
      case t_e of
        Product ts ->
          if i < length ts
            then ts !! i
            else Nothing
        _ -> Nothing

    go (If0 p i e) = do
      t_p <- go yp
      t_i <- go i
      t_e <- go e
      if t_p == Int && t_i == t_e
        then return t_i
        else Nothing

    go (Let recFlag lbinds expr) = infer vars' expr
      where
        vars' = updatevars [(f,t) | (f,_e,t) <- map (dsLocalBind recFlag vars) lbinds] vars
          where
            updatevars [] vars0              = vars0
            updatevars ((f1, t1):rest) vars0 = updatevars rest (Map.insert f1 t1 vars0)

-- Utilities

-- Take a list of Typ's and return T1 -> T2 -> ... -> Tn
joinTyps :: [Typ] -> Typ
joinTyps []     = error "joinTyps: Empty list"
joinTyps [t]    = t
joinTyps (t:ts) = Fun t (joinTyps ts)

class Wrap a where wrap :: ([b] -> a -> a) -> [b] -> a -> a

instance Wrap Typ  where wrap cons xs e = foldr (\x -> cons [x]) e xs
instance Wrap Expr where wrap cons xs t = foldr (\x -> cons [x]) t xs
