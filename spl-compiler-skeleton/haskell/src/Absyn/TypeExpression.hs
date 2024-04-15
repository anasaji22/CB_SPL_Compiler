module Absyn.TypeExpression where

import Absyn.Position
import Absyn.Node
import Data.IORef (IORef)
import Types.Type (Type)
import Types.HasType

data TypeExpression = NamedTypeExpression Position (IORef (Maybe Type)) String -- A named typed expression such as 'int'
                    | ArrayTypeExpression Position (IORef (Maybe Type)) Int TypeExpression -- An array type expression such as 'array [10] of int'

instance Show TypeExpression where
    show :: TypeExpression -> String
    show (NamedTypeExpression _ _ name) = "NamedTypeExpression" ++ format [name]
    show (ArrayTypeExpression _ _ size baseType) = "ArrayTypeExpression" ++ format [show size, show baseType]

instance Node TypeExpression where
    position :: TypeExpression -> Position
    position (NamedTypeExpression position _ _) = position
    position (ArrayTypeExpression position _ _ _) = position

instance HasType TypeExpression where
    dataType :: TypeExpression -> IORef (Maybe Type)
    dataType (NamedTypeExpression _ dataType _) = dataType
    dataType (ArrayTypeExpression _ dataType _ _) = dataType

