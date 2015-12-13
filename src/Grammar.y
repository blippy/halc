{
module Grammar where

import Control.Exception
  
import Exceptions
import Lexer
import Expr


}

%name parz
%error { parseError }
%tokentype { Token }

%token VAR { TkVar $$ }
%token NUM { TkNum $$ }
%token OP  { TkOp  $$ }
%token EQ  { TkEq }

%%

Exprs : {- empty -}   { [] } 
     | Exprs Expr     { $1 ++ [$2] }

Expr : VAR EQ Term { Assign (snd $1) $3 }
     | Term        { ExpTerm $1 }

Term : NUM { Num $1 }
| VAR {  Var $1 }
     | Term OP Term { Binop $2 $1 $3 }


{
parseError :: [Token] -> a
parseError ts =
  let tokeh = show $ head ts in
  throw $ ParseError ("Parse error:" ++ tokeh)
  
}
