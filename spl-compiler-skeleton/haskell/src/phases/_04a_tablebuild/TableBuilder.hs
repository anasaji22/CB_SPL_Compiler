module TableBuilder where

import Table.SymbolTable
import Absyn.Program
import TableInitializer (initializeGlobalTable)
import CommandLineOptions (CommandLineOptions)

buildSymbolTable :: CommandLineOptions -> Program -> IO SymbolTable
buildSymbolTable options program = do
    table <- initializeGlobalTable
    undefined -- TODO (PA4): Implement table building
