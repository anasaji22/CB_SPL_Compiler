module Absyn.Declaration where

import Absyn.Position
import Absyn.Node
import Absyn.Statement
import Absyn.TypeExpression
import Data.Char

-- Any declaration is a `Absyn.Node` and has a name 
class Node a => Declaration a where
    name :: a -> String

-- SPL Local Declarations --

class Declaration a => LocalDeclaration a where
    signature :: a -> TypeExpression

data VariableDefinition = VariableDefinition Position String TypeExpression -- A variable declaration such as 'var a : int'

instance Show VariableDefinition where
    show :: VariableDefinition -> String
    show (VariableDefinition _ name typeExpression) = "VariableDefinition" ++ format [name, show typeExpression]

instance Node VariableDefinition where
    position :: VariableDefinition -> Position
    position (VariableDefinition position _ _) = position

instance Declaration VariableDefinition where
    name :: VariableDefinition -> String
    name (VariableDefinition _ name _) = name

instance LocalDeclaration VariableDefinition where
    signature :: VariableDefinition -> TypeExpression
    signature (VariableDefinition _ _ typeExpression) = typeExpression

data ParameterDefinition = ParameterDefinition Position String TypeExpression Bool -- A parameter declaration such as 'a : int' or 'ref a : int'

instance Show ParameterDefinition where
    show :: ParameterDefinition -> String
    show (ParameterDefinition _ name typeExpression isReference) = "ParameterDefinition" ++ format [name, show typeExpression, map toLower (show isReference)]

instance Node ParameterDefinition where
    position :: ParameterDefinition -> Position
    position (ParameterDefinition position _ _ _) = position

instance Declaration ParameterDefinition where
    name :: ParameterDefinition -> String
    name (ParameterDefinition _ name _ _) = name

instance LocalDeclaration ParameterDefinition where
    signature :: ParameterDefinition -> TypeExpression
    signature (ParameterDefinition _ _ typeExpression _) = typeExpression

isReferenceParameter :: ParameterDefinition -> Bool
isReferenceParameter (ParameterDefinition _ _ _ isReference) = isReference

-- SPL Global Declarations --

data GlobalDeclaration = ProcedureDefinition Position String [ParameterDefinition] [VariableDefinition] [Statement]
                       | TypeDefinition Position String TypeExpression

instance Show GlobalDeclaration where
    show :: GlobalDeclaration -> String
    show (ProcedureDefinition _ name parameters variables statements) = let 
            parametersString = "Parameters" ++ format (map show parameters)
            variablesString = "Variables" ++ format (map show variables)
            bodyString = "Body" ++ format (map show statements)
        in "ProcedureDefinition" ++ format [name, parametersString, variablesString, bodyString]
    show (TypeDefinition _ name typeExpression) = "TypeDefinition" ++ format [name, show typeExpression]

instance Node GlobalDeclaration where
    position :: GlobalDeclaration -> Position
    position (ProcedureDefinition position _ _ _ _) = position
    position (TypeDefinition position _ _) = position

instance Declaration GlobalDeclaration where
    name :: GlobalDeclaration -> String
    name (ProcedureDefinition _ name _ _ _) = name
    name (TypeDefinition _ name _) = name
