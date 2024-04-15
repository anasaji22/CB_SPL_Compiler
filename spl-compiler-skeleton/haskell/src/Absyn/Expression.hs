module Absyn.Expression where

import Absyn.Node
import Absyn.BinaryOperator
import Absyn.UnaryOperator
import Absyn.Position
import Data.IORef
import Types.Type
import Types.HasType

-- SPL Variables --

data Variable = NamedVariable Position (IORef (Maybe Type)) String -- A named variable such as 'i' for example 
              | ArrayAccess Position (IORef (Maybe Type)) Variable Expression -- An array access such as 'a[i + 1]' for example

instance Show Variable where
    show :: Variable -> String
    show (NamedVariable _ _ name)      = "NamedVariable" ++ format [name]
    show (ArrayAccess _ _ array index) = "ArrayAccess" ++ format [show array, show index]

instance Node Variable where
    position :: Variable -> Position
    position (NamedVariable position _ _) = position
    position (ArrayAccess position _ _ _) = position

instance HasType Variable where
    dataType :: Variable -> IORef (Maybe Type)
    dataType (NamedVariable _ dataType _) = dataType
    dataType (ArrayAccess _ dataType _ _) = dataType

-- SPL Expressions --

data Expression = UnaryExpression Position (IORef (Maybe Type)) UnaryOperator Expression
                | BinaryExpression Position (IORef (Maybe Type)) BinaryOperator Expression Expression
                | IntLiteral Position (IORef (Maybe Type)) Int
                | VariableExpression Position (IORef (Maybe Type)) Variable

instance Show Expression where
    show :: Expression -> String
    show (UnaryExpression _ _ op expression)  = "UnaryExpression" ++ format [show op, show expression]
    show (BinaryExpression _ _ op left right) = "BinaryExpression" ++ format [show op, show left, show right]
    show (IntLiteral _ _ value)               = "IntLiteral" ++ format [show value]
    show (VariableExpression _ _ variable)    = "VariableExpression" ++ format [show variable]

instance Node Expression where
    position :: Expression -> Position
    position (UnaryExpression position _ _ _)    = position
    position (BinaryExpression position _ _ _ _) = position
    position (IntLiteral position _ _)           = position
    position (VariableExpression position _ _)   = position

instance HasType Expression where
    dataType :: Expression -> IORef (Maybe Type)
    dataType (UnaryExpression _ dataType _ _)    = dataType
    dataType (BinaryExpression _ dataType _ _ _) = dataType
    dataType (IntLiteral _ dataType _)           = dataType
    dataType (VariableExpression _ dataType _)   = dataType



