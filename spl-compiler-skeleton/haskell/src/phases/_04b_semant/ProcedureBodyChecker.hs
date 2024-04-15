module ProcedureBodyChecker where

import Absyn.Program
import Table.SymbolTable
import Absyn.BinaryOperator
import Absyn.UnaryOperator
import Absyn.Declaration
import Absyn.Expression
import Types.Type
import Absyn.Statement
import Absyn.Position
import Utility.SplError
import Types.HasType (setType)
import Control.Monad (unless, when)

checkProcedures :: Program -> SymbolTable -> IO ()
checkProcedures (Program _ globalDeclarations) globalTable = undefined -- TODO (PA4): Implement procedure body checker
