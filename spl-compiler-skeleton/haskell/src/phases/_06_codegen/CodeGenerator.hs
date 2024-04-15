module CodeGenerator where

import GHC.IO.Handle (Handle)
import CodePrinter (emitImport, emit)
import Absyn.Program (Program (Program))
import Table.SymbolTable (SymbolTable)
import Data.IORef (newIORef)

assemblerProlog :: Handle -> IO ()
assemblerProlog out = do
    emitImport out "printi"
    emitImport out "printc"
    emitImport out "readi"
    emitImport out "readc"
    emitImport out "exit"
    emitImport out "time"
    emitImport out "clearAll"
    emitImport out "setPixel"
    emitImport out "drawLine"
    emitImport out "drawCircle"
    emitImport out "_indexError"

    emit out ""
    emit out "\t.code"
    emit out "\t.align\t4"

generateCode :: Program -> SymbolTable -> Handle -> IO ()
generateCode (Program _ declarations) globalTable out = do
    assemblerProlog out

    undefined -- TODO: Implement code generation



