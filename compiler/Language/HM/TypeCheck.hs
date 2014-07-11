-- Hindley–Milner type system
-- http://en.wikipedia.org/wiki/Hindley%E2%80%93Milner
-- http://web.cs.wpi.edu/~cs4536/c12/milner-damas_principal_types.pdf
module Language.HM.TypeCheck where

import Control.Monad.State
import Data.List        (union, delete, intercalate, nub)
import Data.Maybe       (fromMaybe)

import Language.HM.Syntax
import Language.HM.Parser         (reader)

type Env = [(Var, Poly)]

-- Free Type Variables
free :: Type -> [TVar]
free (TMono (MVar a)) = [a]
free (TMono (MPrim _)) = []
free (TMono (MApp t1 t2)) = free (TMono t1) `union` free (TMono t2)
free (TPoly (PForall a poly)) = delete a (free (TPoly poly))
free (TPoly (PMono t)) = free (TMono t)

freeEnv :: Env -> [TVar]
freeEnv env = concatMap (\(x,s) -> free (TPoly s)) env

type TSubst = [(TVar, Mono)]   

dom :: [(a, b)] -> [a]
dom = map fst

rng :: [(a, b)] -> [b]
rng = map snd

-- TaPL, p. 318; Damas & Milner '82, p. 3
tsubstMono :: TSubst -> Mono -> Mono
tsubstMono s (MVar x) = fromMaybe (MVar x) (lookup x s)
tsubstMono s (MPrim t) = MPrim t
tsubstMono s (MApp t1 t2) = MApp (tsubstMono s t1) (tsubstMono s t2)

-- TODO: implement
isLessSpecialThan :: Poly  -> Poly -> Bool
isLessSpecialThan s1 s2 = error "isLessSpecialThan"

-- Declarative Rule System
typeExp :: Env -> Exp -> Maybe Type
typeExp _env (EVar x) = Just $ TMono (MVar "a") -- lookup x env >>= Just . TPoly
typeExp env (EApp e0 e1) = do
    t0 <- typeExp env e0
    t1 <- typeExp env e1
    case t0 of 
        (TMono (MApp t t')) -> if t1 == TMono t then Just (TMono t') else Nothing
        _ -> Nothing
typeExp env (ELam x e) = do
    t  <- typeExp env (EVar x)
    t' <- typeExp ((x, type2poly t):env) e
    return $ TMono (MApp (type2mono t) (type2mono t'))
typeExp env (ELet x e0 e1) = do
    t0 <- typeExp env e0
    typeExp ((x, type2poly t0):env) e1
-- TODO: missing two cases: [Inst] and [Gen]

type2poly :: Type -> Poly
type2poly (TMono t) = PMono t
type2poly (TPoly s) = s

type2mono :: Type -> Mono
type2mono (TMono t) = t
type2mono (TPoly s) = error "type2mono (TPoly s)"

-- Syntactical Rule System
-- g(tau) = forall a. tau, a = free(t) - free(env)
-- generalize :: Env -> Mono -> Poly
-- quantifies all monotype variables not bound in env

-- Constraint-based typing (TaPL, Figure 22-1)

type Constraint = (CType, CType) 

-- Types used in constraints, different from `Type`
data CType = CTLit
           | CTVar Var
           | CTArr CType CType -- t1 -> t2
           deriving (Eq, Show)

type Context = [(Var, CType)]

-- The type reconstruction monad
type TR = State Int
 
-- Generate a fresh type variable.
uvargen :: TR CType
uvargen = do n <- get
             put $ n + 1
             return $ CTVar ("a" ++ show n)

ctype :: Context -> Exp -> TR ([Constraint], CType)
ctype ctx (EVar x) = 
    case lookup x ctx of 
        Nothing -> error ("Unbound variable: " ++ x)
        Just t -> return ([], t)

ctype ctx (ELit i) = return ([], CTLit)

ctype ctx (EApp e1 e2) = 
    do (c1, t1) <- ctype ctx e1 
       (c2, t2) <- ctype ctx e2
       x <- uvargen
       return (c1 `union` c2 `union` [(t1, CTArr t2 x)], x)

ctype ctx (ELam x e2) = 
    do t1 <- uvargen -- annotate x with some fresh type variable t1
       (c2, t2) <- ctype (ctx `union` [(x, t1)]) e2
       return (c2, CTArr t1 t2)

ctype ctx (EUn op e1) = ctype ctx e1

ctype ctx (EBin op e1 e2) = 
    do (c1, t1) <- ctype ctx e1
       (c2, t2) <- ctype ctx e2
       let c' = if isLitLitOp op then [(t1, CTLit), (t2, CTLit)] else [(t1, t2)]
       return (c1 `union` c2 `union` c', t1)

ctype ctx (EIf0 e1 e2 e3) = 
    do (c1, t1) <- ctype ctx e1
       (c2, t2) <- ctype ctx e2
       (c3, t3) <- ctype ctx e3
       return (c1 `union` c2 `union` c3 `union` [(t1, CTLit), (t2, t3)], t2)

ctype ctx (ELet x e1 e2) = 
    do (c1, t1) <- ctype ctx e1
       (c2, t2) <- ctype ((x, t1):ctx) e2
       return (c1 `union` c2, t2)

ctype ctx (ELetRec bindings e1) = 
    do let vars = map fst bindings
       if length vars /= length (nub vars) 
           then error "Duplicate variable names" 
           else do freshTyVars <- replicateM (length bindings) uvargen
                   let ctx' = zip vars freshTyVars ++ ctx
                   (cs, ts) <- fmap unzip $ zipWithM (\ty (x0, e0) -> do { (c0, t0) <- ctype ctx' e0; return (c0 `union` [(t0, ty)], t0) }) freshTyVars bindings
                   (c1, t1) <- ctype ctx' e1
                   return (foldl union c1 cs, t1)

infer :: Exp -> CType
infer e = 
    let (c, t) = evalState (ctype [] e) 0 in 
    let s = unify c in
    subst s t

type Substitution = (Var, CType)

fv :: CType -> [Var]
fv CTLit = []
fv (CTVar x) = [x]
fv (CTArr t1 t2) = nub $ (fv t1) ++ (fv t2)

class Subst a where
    subst :: [Substitution] -> a -> a

instance Subst CType where
    subst _s CTLit = CTLit
    subst s (CTVar x) = fromMaybe (CTVar x) (lookup x s)
    subst s (CTArr t1 t2) = CTArr (subst s t1) (subst s t2)

-- Composition of substitution s1 and s2
composeS :: [Substitution] -> [Substitution] -> [Substitution]
composeS s1 s2 = mapping1 ++ mapping2
    where mapping1 = map (\(x, t) -> (x, subst s1 t)) s2
          mapping2 = map (\x -> x) (filter (\(x, t) -> not (x `elem` dom s2)) s1)

substC :: [Substitution] -> [Constraint] -> [Constraint]
substC s c = map (substC0 s) c
    where substC0 s (t1, t2) = (subst s t1, subst s t2)

double :: String
double = "\\f -> (\\x -> f(f(x)))"

unify :: [Constraint] -> [Substitution]
unify [] = []
unify c@((s, t):c')
    | s == t = unify c'
    | otherwise = case (s, t) of
        (CTVar x, _) -> if x `elem` fv t then raise else unify (substC [(x, t)] c') `composeS` [(x, t)] 
        (_, CTVar x) -> if x `elem` fv s then raise else unify (substC [(x, s)] c') `composeS` [(x, s)] 
        (CTArr s1 s2, CTArr t1 t2) -> unify $ c' ++ [(s1, t1), (s2, t2)]
        _ -> raise
    where 
        raise = error $ "Cannot unify " ++ show s ++ " and " ++ show t ++ " given constraints:\n" ++ show c