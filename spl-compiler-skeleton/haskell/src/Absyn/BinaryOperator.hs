module Absyn.BinaryOperator where

import Prelude hiding (EQ, LT, GT)

data BinaryOperator = ADD | SUB | MUL | DIV | EQU | NEQ | LST | LSE | GRT | GRE deriving (Eq, Show)

operatorString :: BinaryOperator -> String
operatorString ADD = "+"
operatorString SUB = "-"
operatorString MUL = "*"
operatorString DIV = "/"
operatorString EQU = "="
operatorString NEQ = "#"
operatorString LST = "<"
operatorString LSE = "<="
operatorString GRT = ">"
operatorString GRE = ">="

isArithmeticOperator :: BinaryOperator -> Bool
isArithmeticOperator ADD = True
isArithmeticOperator SUB = True
isArithmeticOperator MUL = True
isArithmeticOperator DIV = True
isArithmeticOperator _   = False

isEqualityOperator :: BinaryOperator -> Bool
isEqualityOperator EQU = True
isEqualityOperator NEQ = True
isEqualityOperator _   = False

isComparisonOperator :: BinaryOperator -> Bool
isComparisonOperator LST = True
isComparisonOperator LSE = True
isComparisonOperator GRT = True
isComparisonOperator GRE = True
isComparisonOperator op  = isEqualityOperator op

flipComparisonOperator :: BinaryOperator -> BinaryOperator
flipComparisonOperator EQU = NEQ
flipComparisonOperator NEQ = EQU
flipComparisonOperator LST = GRE
flipComparisonOperator LSE = GRT
flipComparisonOperator GRT = LSE
flipComparisonOperator GRE = LST
flipComparisonOperator op  = op 
