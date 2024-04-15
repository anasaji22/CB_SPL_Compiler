module Table.SymbolTable where

import Control.Monad
import Data.IORef
import Data.List
import Data.Map qualified as Map
import StackLayout
import Types.Type
import Utility.SplError
import Absyn.Position (unknown)

data SymbolTable = SymbolTable
  { entries :: IORef (Map.Map String Entry),
    upperLevel :: Maybe SymbolTable
  }

printTable :: SymbolTable -> IO ()
printTable table = printTableRecursively (Just table) 0
  where
    printTableRecursively :: Maybe SymbolTable -> Int -> IO ()
    printTableRecursively Nothing _ = putStr ""
    printTableRecursively (Just table) level = do
      putStrLn $ "  level " ++ show level
      entries <- readIORef (entries table)
      if Map.null entries
        then putStrLn "    <empty>"
        else do
          let sortedEntries = sortBy (\(a, _) (b, _) -> compare a b) $ Map.toList entries
          mapM_
            ( \(name, value) -> do
                putStrLn $ "    " ++ name ++ " --> " ++ show value
            )
            sortedEntries
      printTableRecursively (upperLevel table) (level + 1)

newSymbolTable :: Maybe SymbolTable -> IO SymbolTable
newSymbolTable upperLevel = do
  mapping <- newIORef Map.empty
  return (SymbolTable mapping upperLevel)

enter :: SymbolTable -> String -> Entry -> IO ()
enter table name entry = do
  mapping <- readIORef (entries table)
  writeIORef (entries table) $ Map.insert name entry mapping

enterWithError :: SymbolTable -> String -> Entry -> SplError -> IO ()
enterWithError table name entry error = do
  entries <- readIORef (entries table)
  if Map.member name entries
    then throw error
    else enter table name entry

lookup :: SymbolTable -> String -> IO (Maybe Entry)
lookup table name = do
  entries <- readIORef (entries table)
  case Map.lookup name entries of
    Just entry -> return $ Just entry
    Nothing -> join <$> mapM (`Table.SymbolTable.lookup` name) (upperLevel table)

lookupWithError :: SymbolTable -> String -> SplError -> IO Entry
lookupWithError table name error = do
  entry <- Table.SymbolTable.lookup table name
  case entry of
    Just entry' -> return entry'
    Nothing -> throw error

lookupOrThrow :: SymbolTable -> String -> IO Entry
lookupOrThrow table name = lookupWithError table name (UndefinedIdentifier unknown name)

data ParameterType = ParameterType Type Bool (IORef (Maybe Int))

instance Show ParameterType where
  show :: ParameterType -> String
  show (ParameterType type' isReference _) = if isReference then "ref " ++ show type' else show type'

-- Entries --

data Entry
  = VariableEntry Type Bool (IORef (Maybe Int))
  | ProcedureEntry SymbolTable [ParameterType] StackLayout
  | TypeEntry Type

instance Show Entry where
  show :: Entry -> String
  show (VariableEntry type' isReference _) = "var: " ++ (if isReference then "ref " else "") ++ show type'
  show (ProcedureEntry _ parameterTypes _) = "proc: (" ++ intercalate ", " (map show parameterTypes) ++ ")"
  show (TypeEntry type') = "type: " ++ show type'

predefinedProcedureEntry :: [ParameterType] -> Int -> IO Entry
predefinedProcedureEntry parameterTypes argumentAreaSize = do
  symbolTable <- newSymbolTable Nothing
  argumentsSize <- newIORef $ Just argumentAreaSize
  localVariables <- newIORef Nothing
  outgoingArea <- newIORef Nothing
  return $ ProcedureEntry symbolTable parameterTypes $ StackLayout argumentsSize localVariables outgoingArea
