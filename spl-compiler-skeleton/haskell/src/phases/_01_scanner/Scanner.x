{
module Scanner where

import qualified Absyn.Position as Position
import Token
import Data.Char(ord)
import System.IO.Unsafe
import Utility.SplError

}

%wrapper "posn"
%encoding "utf-8"

tokens :-  
  "var" { token Token.VAR }
  -- TODO (PA1): Implement remaining tokens
  
{
tokenWithLexeme :: (String -> TokenClass) -> (AlexPosn -> String -> Token)
tokenWithLexeme action (AlexPn _ line column) lexeme = Token (Position.Position line column) (action lexeme)

token :: TokenClass -> (AlexPosn -> String -> Token)
token tokenClass = tokenWithLexeme (const tokenClass)
}
