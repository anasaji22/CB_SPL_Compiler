module Main where

import Utility.SplError
import qualified Absyn.Position as Position
import System.IO
import Scanner
import Token
import Parser
import TableBuilder (buildSymbolTable)
import ProcedureBodyChecker (checkProcedures)
import VarAllocator (allocVars)
import CodeGenerator (generateCode)
import System.Environment (getArgs)
import Control.Monad (when)
import System.Exit (exitSuccess)
import CommandLineOptions

main :: IO ()
main = do
    options <- getArgs >>= parseCommandLineOptions

    file <- openFile (inFileName options) ReadMode
    contents <- hGetContents file

    tokens <- scan contents
    when (phaseOption options == TOKENS) $ do
        mapM_ print tokens
        exitSuccess

    program <- parse tokens
    when (phaseOption options == PARSE) $ do
        putStrLn "Input parsed successfully!"
        exitSuccess
    when (phaseOption options == ABSYN) $ do
        print program
        exitSuccess

    globalTable <- buildSymbolTable options program
    when (phaseOption options == TABLES) exitSuccess

    checkProcedures program globalTable
    when (phaseOption options == SEMANT) $ do
        putStrLn "No semantic errors found!"
        exitSuccess

    allocVars options program globalTable
    when (phaseOption options == VARS) exitSuccess

    generateCode program globalTable (outputWriter options)
    hClose (outputWriter options)
    hClose file
    return ()

scan :: String -> IO [Token]
scan contents = let
        go :: AlexInput -> IO [Token]
        go input@(position, _, _, contents) = case alexScan input 0 of
            AlexEOF                                          -> return []
            AlexError (AlexPn _ line column, _, _, [])       -> throw $ LexicalError (Position.Position line column) '\0'
            AlexError (AlexPn _ line column, _, _, contents) -> throw $ LexicalError (Position.Position line column) (head contents)
            AlexSkip input _                                 -> go input
            AlexToken input length action                    -> (action position (take length contents) :) <$> go input
    in go (alexStartPos, '\n', [], contents)

