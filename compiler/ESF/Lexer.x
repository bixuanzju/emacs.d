{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ESF.Lexer
    ( lexer
    , Token(..)
    ) where

import qualified Language.Java.Syntax as J (Op(..))
import JVMTypeQuery
}

%wrapper "posn"

$alpha = [A-Za-z]
$digit = [0-9]

$vchar = [$alpha $digit \_ \']

tokens :-

    $white+     ;
    "#".*       ;
    "--".*      ;
    "//".*      ;

    \(          { \_ _ -> Toparen }
    \)          { \_ _ -> Tcparen }
    \/\\        { \_ _ -> Ttlam }
    \\          { \_ _ -> Tlam }
    \:          { \_ _ -> Tcolon }
    forall      { \_ _ -> Tforall }
    \-\>        { \_ _ -> Tarrow }
    \.          { \_ _ -> Tdot }
    let         { \_ _ -> Tlet }
    rec         { \_ _ -> Trec }
    \=          { \_ _ -> Teq }
    and         { \_ _ -> Tand }
    in          { \_ _ -> Tin }
    Int         { \_ _ -> Tint }
    if0         { \_ _ -> Tif0 }
    then        { \_ _ -> Tthen }
    else        { \_ _ -> Telse }
    \,          { \_ _ -> Tcomma }
    new         { \_ _ -> Tnew }

    -- java.package.path.Classname
    ([a-z] [$vchar]* \.)+ [A-Z] [$vchar]*  { \_ s -> Tjavaclass s }

    [A-Z] [$vchar]*     { \_ s -> Tupperid s }
    [a-z] [$vchar]*     { \_ s -> Tlowerid s }
    \_ $digit+          { \_ s -> Tunderid (read (tail s))  }

    $digit+     { \_ s -> Tinteger (read s) }

    -- http://hackage.haskell.org/package/language-java-0.2.5/docs/src/Language-Java-Syntax.html#Op
    \*          { \_ _ -> Tprimop J.Mult   }
    \/          { \_ _ -> Tprimop J.Div    }
    \%          { \_ _ -> Tprimop J.Rem    }
    \+          { \_ _ -> Tprimop J.Add    }
    \-          { \_ _ -> Tprimop J.Sub    }
    \<          { \_ _ -> Tprimop J.LThan  }
    \<\=        { \_ _ -> Tprimop J.LThanE }
    \>          { \_ _ -> Tprimop J.GThan  }
    \>\=        { \_ _ -> Tprimop J.GThanE }
    \=\=        { \_ _ -> Tprimop J.Equal  }
    \!\=        { \_ _ -> Tprimop J.NotEq  }
    \&\&        { \_ _ -> Tprimop J.CAnd   }
    \|\|        { \_ _ -> Tprimop J.COr    }

{
data Token = Toparen | Tcparen
           | Ttlam | Tlam | Tcolon | Tforall | Tarrow | Tdot
           | Tlet | Trec | Teq | Tand | Tin
           | Tint | Tjavaclass String
           | Tnew
           | Tif0 | Tthen | Telse
           | Tcomma
           | Tupperid String | Tlowerid String | Tunderid Int
           | Tinteger Integer
           | Tprimop J.Op
           deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}