{
module Parser where

import System.IO
import Token
import Utility.SplError
import Absyn.Declaration
import Absyn.TypeExpression
import Absyn.Expression
import Absyn.Program
import Absyn.Statement
import Absyn.Node
import Data.IORef
import qualified Absyn.UnaryOperator as UnaryOperator
import qualified Absyn.BinaryOperator as BinaryOperator
import qualified Absyn.Position as Position
import Data.Maybe (listToMaybe)

}

%name parse
%tokentype { Token }
%error { parseError }
%monad { IO } { >>= } { return }

%token
    LBRACK   { Token $$ Token.LBRACK }
    RBRACK   { Token $$ Token.RBRACK }
    LPAREN   { Token $$ Token.LPAREN }
    RPAREN   { Token $$ Token.RPAREN }
    LCURL    { Token $$ Token.LCURL }
    RCURL    { Token $$ Token.RCURL }
    LT       { Token $$ Token.LT }
    LE       { Token $$ Token.LE }
    GT       { Token $$ Token.GT }
    GE       { Token $$ Token.GE }
    NE       { Token $$ Token.NE }
    EQ       { Token $$ Token.EQ }
    PLUS     { Token $$ Token.PLUS }
    MINUS    { Token $$ Token.MINUS }
    STAR     { Token $$ Token.STAR }
    SLASH    { Token $$ Token.SLASH }
    ASGN     { Token $$ Token.ASGN }
    COMMA    { Token $$ Token.COMMA }
    COLON    { Token $$ Token.COLON }
    SEMIC    { Token $$ Token.SEMIC }
    TYPE     { Token $$ Token.TYPE }
    PROC     { Token $$ Token.PROC }
    ARRAY    { Token $$ Token.ARRAY }
    OF       { Token $$ Token.OF }
    REF      { Token $$ Token.REF }
    VAR      { Token $$ Token.VAR }
    IF       { Token $$ Token.IF }
    ELSE     { Token $$ Token.ELSE }
    WHILE    { Token $$ Token.WHILE }
    IDENT    { Token position (Token.IDENT $$) }
    INTLIT   { Token position (Token.INTLIT $$) }

%%
 
program : INTLIT { undefined }-- TODO (PA2): This is a placeholder. Implement actual grammar
 
{
parseError :: [Token] -> IO a
parseError tokens = throw $ SyntaxError Position.unknown (listToMaybe tokens)
}
