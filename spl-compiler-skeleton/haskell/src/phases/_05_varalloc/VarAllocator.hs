module VarAllocator where

import Absyn.Program
import Data.IORef (readIORef)
import StackLayout (StackLayout (argumentAreaSize, localVarAreaSize, outgoingAreaSize), frameSize)
import Table.SymbolTable
import CommandLineOptions (CommandLineOptions (phaseOption), PhaseOption (VARS))
import Control.Monad (when)
import Absyn.Declaration (GlobalDeclaration(ProcedureDefinition))

referenceByteSize :: Int
referenceByteSize = 4

printFormattedVars :: SymbolTable -> Program -> IO ()
printFormattedVars table (Program _ declarations) = do
  mapM_ (printStackLayout table) declarations
  where
    printStackLayout table (ProcedureDefinition _ name _ _ _) = do
      entry <- lookupOrThrow table name
      case entry of
        (ProcedureEntry _ _ stackLayout) -> do
          putStrLn $ "Variable allocation for procedure '" ++ name ++ "':"
          readIORef (argumentAreaSize stackLayout) >>= \x -> putStrLn $ "  - size of argument area = " ++ maybe "NULL" show x
          readIORef (localVarAreaSize stackLayout) >>= \x -> putStrLn $ "  - size of localvar area = " ++ maybe "NULL" show x
          readIORef (outgoingAreaSize stackLayout) >>= \x -> putStrLn $ "  - size of outgoing area = " ++ maybe "NULL" show x
          frameSize stackLayout >>= \x -> putStrLn $ "  - frame size = " ++ show x

        _ -> return ()
    printStackLayout _ _ = return ()
    
allocVars :: CommandLineOptions -> Program -> SymbolTable -> IO ()
allocVars options program@(Program _ declarations) table = do
  undefined -- TODO PA5: Implement variable allocation
