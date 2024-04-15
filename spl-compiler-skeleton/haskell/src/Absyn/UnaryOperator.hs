module Absyn.UnaryOperator where

data UnaryOperator = MINUS deriving (Eq, Show)

operatorString :: UnaryOperator -> String
operatorString MINUS = "-"

