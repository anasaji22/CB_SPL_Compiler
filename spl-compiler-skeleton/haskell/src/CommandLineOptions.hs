module CommandLineOptions where

import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust, isNothing)
import GHC.IO.Handle (Handle)
import GHC.IO.Handle.FD (openFile, stderr, stdout)
import GHC.IO.Handle.Text (hPutStrLn)
import GHC.IO.IOMode (IOMode (WriteMode))
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

data PhaseOption = TOKENS | PARSE | ABSYN | TABLES | SEMANT | VARS | ALL deriving (Show, Eq)

data CommandLineOptions = CommandLineOptions {phaseOption :: PhaseOption, inFileName :: String, outFileName :: Maybe String, outputWriter :: Handle} deriving (Show)

usageError :: String -> IO ()
usageError message = do
  hPutStrLn stderr message
  showUsage stderr
  exitWith $ ExitFailure 1

showUsage :: Handle -> IO ()
showUsage out = do
  hPutStrLn out "Usage: 'spl' [OPTION] INPUT_FILE [OUTPUT_FILE]"
  hPutStrLn out ""
  hPutStrLn out "Executes all compiler phases up to (and including) the specified one."
  hPutStrLn out "If no flag is specified, all phases are run and code is written to the output file."
  hPutStrLn out "Options:"
  hPutStrLn out "  --tokens            Phase 1: Scans for tokens and prints them."
  hPutStrLn out "  --parse             Phase 2: Parses the stream of tokens to check for syntax errors."
  hPutStrLn out "  --absyn             Phase 3: Creates an abstract syntax tree from the input tokens and prints it."
  hPutStrLn out "  --tables            Phase 4a: Builds a symbol table and prints its entries."
  hPutStrLn out "  --semant            Phase 4b: Performs the semantic analysis."
  hPutStrLn out "  --vars              Phase 5: Allocates memory space for variables and prints the amount of allocated memory."
  hPutStrLn out "  --help              Show this help."

parsePhaseOption :: [String] -> PhaseOption
parsePhaseOption args = case () of
  _
    | "--tokens" `elem` args -> TOKENS
    | "--parse" `elem` args -> PARSE
    | "--absyn" `elem` args -> ABSYN
    | "--tables" `elem` args -> TABLES
    | "--semant" `elem` args -> SEMANT
    | "--vars" `elem` args -> VARS
  _ -> ALL

findNth :: Int -> (a -> Bool) -> [a] -> Maybe a
findNth _ _ [] = Nothing
findNth n predicate xs = case drop n $ filter predicate xs of
  [] -> Nothing
  (x : _) -> Just x

parseCommandLineOptions :: [String] -> IO CommandLineOptions
parseCommandLineOptions args = do
  when ("--help" `elem` args) $ do
    showUsage stdout
    exitSuccess

  let inFileName = findNth 0 nonPrefixed args
  let outFileName = findNth 1 nonPrefixed args
  unless (isNothing $ findNth 2 nonPrefixed args) $ usageError "Too many positional arguments!"
  outputWriter <- case outFileName of
    (Just path) -> openFile path WriteMode
    Nothing -> pure stdout

  when (isNothing inFileName) $ usageError "No input file!"

  return $ CommandLineOptions (parsePhaseOption args) (fromJust inFileName) outFileName outputWriter
  where
    nonPrefixed e = not $ "--" `isPrefixOf` e