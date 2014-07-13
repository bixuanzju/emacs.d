{
{-# LANGUAGE RecordWildCards #-}

module Language.ESF.Parser where

import qualified Language.Java.Syntax as J (Op (..))

import Language.ESF.Syntax
import Language.ESF.Lexer
}

%name parser
%tokentype { Token }
%monad     { P }
%error     { parseError }

%token

  "("      { Toparen }
  ")"      { Tcparen }
  "/\\"    { Ttlam }
  "\\"     { Tlam }
  ":"      { Tcolon }
  "forall" { Tforall }
  "->"     { Tarrow }
  "."      { Tdot }
  "let"    { Tlet }
  "rec"    { Trec }
  "="      { Teq }
  "and"    { Tand }
  "in"     { Tin }
  "Int"    { Tint }
  "if0"    { Tif0 }
  "then"   { Tthen }
  "else"   { Telse }
  ","      { Tcomma }

  UPPERID  { Tupperid $$ }
  LOWERID  { Tlowerid $$ }
  UNDERID  { Tunderid $$ }

  INTEGER  { Tinteger $$ }

  "*"      { Tprimop J.Mult   }
  "/"      { Tprimop J.Div    }
  "%"      { Tprimop J.Rem    }
  "+"      { Tprimop J.Add    }
  "-"      { Tprimop J.Sub    }
  "<"      { Tprimop J.LThan  }
  "<="     { Tprimop J.LThanE }
  ">"      { Tprimop J.GThan  }
  ">="     { Tprimop J.GThanE }
  "=="     { Tprimop J.Equal  }
  "!="     { Tprimop J.NotEq  }
  "&&"     { Tprimop J.CAnd   }
  "||"     { Tprimop J.COr    }

-- Precedence and associativity directives
%nonassoc EOF

%right "in"
%right "->"
%nonassoc "else"

-- http://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
%left "||"
%left "&&"
%nonassoc "==" "!="
%nonassoc "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" "%"
%nonassoc UMINUS

%%

-- There are times when it is more convenient to parse a more general
-- language than that which is actually intended, and check it later.

-- Reference for rules:
-- https://github.com/ghc/ghc/blob/master/compiler/parser/Parser.y.pp#L1453

expr :: { Expr }
     : infixexpr %prec EOF      { $1 }

infixexpr :: { Expr }
    : expr10                    { $1 }
    | infixexpr "*"  infixexpr  { PrimOp J.Mult   $1 $3 }
    | infixexpr "/"  infixexpr  { PrimOp J.Div    $1 $3 }
    | infixexpr "%"  infixexpr  { PrimOp J.Rem    $1 $3 }
    | infixexpr "+"  infixexpr  { PrimOp J.Add    $1 $3 }
    | infixexpr "-"  infixexpr  { PrimOp J.Sub    $1 $3 }
    | infixexpr "<"  infixexpr  { PrimOp J.LThan  $1 $3 }
    | infixexpr "<=" infixexpr  { PrimOp J.LThanE $1 $3 }
    | infixexpr ">"  infixexpr  { PrimOp J.GThan  $1 $3 }
    | infixexpr ">=" infixexpr  { PrimOp J.GThanE $1 $3 }
    | infixexpr "==" infixexpr  { PrimOp J.Equal  $1 $3 }
    | infixexpr "!=" infixexpr  { PrimOp J.NotEq  $1 $3 }
    | infixexpr "&&" infixexpr  { PrimOp J.CAnd   $1 $3 }
    | infixexpr "||" infixexpr  { PrimOp J.COr    $1 $3 }

expr10 :: { Expr }
    : "/\\" tvar "." expr                    { BLam $2 $4  }
    | "\\" "(" var ":" typ ")" "." expr      { Lam ($3, $5) $8 }
    | "let" recflag and_localbinds "in" expr { Let $2 $3 $5 }
    | "if0" expr "then" expr "else" expr     { If0 $2 $4 $6 }
    | "-" INTEGER %prec UMINUS               { Lit (Integer (-$2)) }
    | fexp                                   { $1 }

fexp :: { Expr }
    : fexp aexp         { App  $1 $2 }
    | fexp typ          { TApp $1 $2 }
    | aexp              { $1 }

aexp :: { Expr }
    : aexp1             { $1 }

aexp1 :: { Expr }
    : aexp2             { $1 }

aexp2 :: { Expr }
    : var                       { Var $1 }
    | INTEGER                   { Lit (Integer $1) }
    | aexp "." UNDERID          { Proj $1 $3 }
    | "(" comma_exprs ")"       { Tuple $2 }
    | "(" expr ")"              { $2 }

comma_exprs :: { [Expr] }
    : expr "," expr             { [$1, $3] }
    | expr "," comma_exprs      { $1:$3    }

localbind :: { LocalBind }
    : var tvars var_annots "=" expr
        { LocalBind { local_id = $1
                    , local_targs = $2
                    , local_args = $3
                    , local_rhs = $5
                    }
        }

and_localbinds :: { [LocalBind] }
    : localbind "and" localbind      { [$1, $3] }
    | localbind "and" and_localbinds { $1:$3    }

recflag :: { RecFlag }
    : "rec"       { Rec }
    | {- empty -} { NonRec }

typ :: { Typ }
    : "forall" tvar tvars "." typ       { Forall ($2:$3) $5 }

    -- Require an atyp on the LHS so that `for A. A -> A` cannot be parsed
    -- as `(for A. A) -> A`, since `for A. A` is not a valid atyp.
    | atyp "->" typ     { Fun $1 $3 }

    | atyp              { $1 }

comma_typs :: { [Typ] }
    : typ "," typ               { $1:[$3] }
    | typ "," comma_typs        { $1:$3   }

atyp :: { Typ }
    : tvar                      { TVar $1 }
    | "Int"                     { Int }
    | "(" typ ")"               { $2 }
    | "(" comma_typs ")"        { Product $2 }

var :: { String }
    : LOWERID           { $1 }

tvar :: { String }
    : UPPERID           { $1 }

tvars :: { [String] }
    : tvar tvars        { $1:$2 }
    | {- empty -}       { []    }

var_annot :: { (String, Typ) }
    : "(" var ":" typ ")"  { ($2, $4) }

var_annots :: { [(String, Typ)] }
    : var_annot var_annots      { $1:$2 }
    | {- empty -}               { []    }

{
-- The monadic parser
data P a = POk a | PError String

instance Monad P where
    POk x      >>= f = f x
    PError msg >>= f = PError msg
    return x         = POk x

parseError :: [Token] -> P a
parseError tokens = PError ("Parse error before tokens:\n\t" ++ show tokens)

reader :: String -> Expr
reader src = case (parser . lexer) src of
                 POk expr   -> expr
                 PError msg -> error msg
}