module Utility.SplError where

import Absyn.Position
import System.Exit
import System.IO
import Token
import Data.Char(isSpace, isControl, ord)
import Numeric (showHex)
import Types.Type (Type)
import qualified Absyn.Position as Position
import Absyn.BinaryOperator (BinaryOperator)
import Absyn.UnaryOperator (UnaryOperator)

data SplError = UndefinedIdentifier Position String
              | SyntaxError Position (Maybe Token)
              | LexicalError Position Char
              | NotAType Position String
              | RedefinitionOfIdentifier Position String
              | ParameterMustBeReference Position String Type
              | MainIsMissing
              | MainIsNotAProcedure
              | MainMustNotHaveParameters
              | NotAVariable Position String
              | IndexTypeMismatch Position Type
              | IndexingNonArray Position Type
              | BinaryOperandTypeMismatch Position BinaryOperator Type Type
              | UnaryOperandTypeMismatch Position UnaryOperator Type
              | IllegalAssignment Position Type Type
              | IfConditionMustBeBoolean Position Type
              | WhileConditionMustBeBoolean Position Type
              | CallOfNonProcedure Position String
              | ArgumentCountMismatch Position String Int Int
              | ArgumentTypeMismatch Position String Int Type Type
              | ArgumentMustBeAVariable Position String Int
              | RegisterOverflow

getErrorMessage :: SplError -> String
getErrorMessage (UndefinedIdentifier _ name)                                = "Identifier '" ++ name ++ "' is not defined."
getErrorMessage (SyntaxError _ unexpected)                                  = case unexpected of
                                                                                (Just token) -> "Syntax error: Unexpected token '" ++ show token ++ "'."
                                                                                Nothing -> "Syntax error: Unexpected token 'EOF'."
getErrorMessage (LexicalError _ char) | isSpace char || isControl char      = "Lexical error: Unexpected character with ascii code 0x" ++ (flip showHex "" . ord) char ++ "."
                                      | otherwise                           = "Lexical error: Unexpected character '" ++ (char : "'.")
getErrorMessage (NotAType _ name)                                           = "Identifier '" ++ name ++ "' does not refer to a type."
getErrorMessage (RedefinitionOfIdentifier _ name)                           = "Identifier '" ++ name ++ "' is already defined in this scope."
getErrorMessage (ParameterMustBeReference _ name type')                     = "Non-reference parameter '" ++ name ++ "' has type '" ++ show type' ++ "', which can only be passed by reference."
getErrorMessage MainIsMissing                                               = "Procedure 'main' is missing."
getErrorMessage MainIsNotAProcedure                                         = "Identifier 'main' does not refer to a procedure."
getErrorMessage MainMustNotHaveParameters                                   = "Procedure 'main' must not have any parameters."
getErrorMessage (NotAVariable _ name)                                       = "Identifier '" ++ name ++ "' does not refer to a variable."
getErrorMessage (IndexTypeMismatch _ type')                                 = "Type mismatch: Array index expected to be of type 'int', but is type '" ++ show type' ++ "."
getErrorMessage (IndexingNonArray _ type')                                  = "Type mismatch: Invalid array access operation on non-array variable of type '" ++ show type' ++ "'."
getErrorMessage (BinaryOperandTypeMismatch _ operator left right)           = "Type mismatch in binary expression: Operator '" ++ show operator ++ "' does not accept operands of types '" ++ show left ++ "' and '" ++ show right ++ "'."
getErrorMessage (UnaryOperandTypeMismatch _ operator right)                 = "Type mismatch in unary expression: Operator '" ++ show operator ++ "' does not accept operand of type '" ++ show right ++ "'."
getErrorMessage (IllegalAssignment _ variableType valueType)                = "A value of type '" ++ show valueType ++ "' can not be assigned to variable of type '" ++ show variableType ++ "'."
getErrorMessage (IfConditionMustBeBoolean _ actual)                         = "if' condition expected to be of type 'boolean', but is of type '" ++ show actual ++ "'."
getErrorMessage (WhileConditionMustBeBoolean _ actual)                      = "while' condition expected to be of type 'boolean', but is of type '" ++ show actual ++ "'."
getErrorMessage (CallOfNonProcedure _ name)                                 = "Identifier '" ++ name ++ "' does not refer to a procedure."
getErrorMessage (ArgumentCountMismatch _ name expected actual)              = if actual < expected then 
                                                                                "Argument count mismatch: Procedure '" ++ name ++ "' expects " ++ show expected ++ " arguments, but only " ++ show actual ++ " were provided." 
                                                                              else
                                                                                 "Argument count mismatch: Procedure '" ++ name ++ "' expects only " ++ show expected ++ " arguments, but " ++ show actual ++ " were provided."
getErrorMessage (ArgumentTypeMismatch _ name argumentIndex expected actual) = "Argument type mismatch in call of procedure '" ++ name ++ "'. Argument " ++ show argumentIndex ++ " is expected to have type '" ++ show expected ++ "', but has type '" ++ show actual ++ "'."
getErrorMessage (ArgumentMustBeAVariable _ name argumentIndex)              = "Invalid argument for reference parameter in call to procedure '" ++ name ++ "': Argument " ++ show argumentIndex ++ " must be a variable."
getErrorMessage RegisterOverflow                                            = "There are not enough registers to run this program!"


getErrorCode :: SplError -> Int
getErrorCode UndefinedIdentifier {}         = 101
getErrorCode SyntaxError {}                 = 100
getErrorCode LexicalError {}                = 99
getErrorCode NotAType {}                    = 102
getErrorCode RedefinitionOfIdentifier {}    = 103
getErrorCode ParameterMustBeReference {}    = 104
getErrorCode MainIsMissing {}               = 125
getErrorCode MainIsNotAProcedure {}         = 126
getErrorCode MainMustNotHaveParameters {}   = 127
getErrorCode NotAVariable {}                = 122
getErrorCode IndexTypeMismatch {}           = 124
getErrorCode IndexingNonArray {}            = 123
getErrorCode BinaryOperandTypeMismatch {}   = 118
getErrorCode UnaryOperandTypeMismatch {}    = 119
getErrorCode IllegalAssignment {}           = 108
getErrorCode IfConditionMustBeBoolean {}    = 110
getErrorCode WhileConditionMustBeBoolean {} = 111
getErrorCode CallOfNonProcedure {}          = 113
getErrorCode ArgumentCountMismatch {}       = 116
getErrorCode ArgumentTypeMismatch {}        = 114
getErrorCode ArgumentMustBeAVariable {}     = 116
getErrorCode RegisterOverflow               = 140

getErrorPosition :: SplError -> Position 
getErrorPosition (UndefinedIdentifier position _)           = position
getErrorPosition (SyntaxError position _)                   = position
getErrorPosition (LexicalError position _)                  = position
getErrorPosition (NotAType position _)                      = position
getErrorPosition (RedefinitionOfIdentifier position _)      = position
getErrorPosition (ParameterMustBeReference position _ _)    = position
getErrorPosition MainIsMissing                              = Position.unknown
getErrorPosition MainIsNotAProcedure                        = Position.unknown
getErrorPosition MainMustNotHaveParameters                  = Position.unknown
getErrorPosition (NotAVariable position _)                  = position
getErrorPosition (IndexTypeMismatch position _)             = position
getErrorPosition (IndexingNonArray position _)              = position
getErrorPosition (BinaryOperandTypeMismatch position _ _ _) = position
getErrorPosition (UnaryOperandTypeMismatch position _ _)    = position
getErrorPosition (IllegalAssignment position _ _)           = position
getErrorPosition (IfConditionMustBeBoolean position _)      = position
getErrorPosition (WhileConditionMustBeBoolean position _)   = position
getErrorPosition (CallOfNonProcedure position _)            = position
getErrorPosition (ArgumentCountMismatch position _ _ _)     = position
getErrorPosition (ArgumentTypeMismatch position _ _ _ _)    = position
getErrorPosition (ArgumentMustBeAVariable position _ _)     = position
getErrorPosition RegisterOverflow                           = Position.unknown

throw :: SplError -> IO a
throw error = do
    hPutStr stderr "An error occurred at Line "
    let Position line column = getErrorPosition error
    hPutStr stderr (show line)
    hPutStr stderr ", Column "
    hPutStr stderr (show column)
    hPutStrLn stderr ":"
    hPutStrLn stderr (getErrorMessage error)
    exitWith (ExitFailure $ getErrorCode error)
