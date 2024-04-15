module CodePrinter where

import GHC.IO.Handle (Handle, hPutStr)
import Register (Register)
import GHC.IO.Handle.Text (hPutStrLn)

emitRRR :: Handle -> String -> Register -> Register -> Register -> IO ()
emitRRR out opcode r1 r2 r3 = hPutStr out $ "\t" ++ opcode ++ "\t" ++ show r1 ++ "," ++ show r2 ++ "," ++ show r3 ++ "\n"

emitRRI :: Handle -> String -> Register -> Register -> Int -> IO ()
emitRRI out opcode r1 r2 value = hPutStr out $ "\t" ++ opcode ++ "\t" ++ show r1 ++ "," ++ show r2 ++ "," ++ show value ++ "\n"

emitRRL :: Handle -> String -> Register -> Register -> String -> IO ()
emitRRL out opcode r1 r2 label = hPutStr out $ "\t" ++ opcode ++ "\t" ++ show r1 ++ "," ++ show r2 ++ "," ++ label ++ "\n"

emitR :: Handle -> String -> Register -> IO ()
emitR out opcode r1 = hPutStr out $ "\t" ++ opcode ++ "\t" ++ show r1 ++ "\n"

emitL :: Handle -> String -> String -> IO ()
emitL out opcode label = hPutStr out $ "\t" ++ opcode ++ "\t" ++ label ++ "\n"

emitRRRC :: Handle -> String -> Register -> Register -> Register -> String -> IO ()
emitRRRC out opcode r1 r2 r3 comment = hPutStr out $ "\t" ++ opcode ++ "\t" ++ show r1 ++ "," ++ show r2 ++ "," ++ show r3 ++ "\t\t; " ++ comment ++ "\n"

emitRRIC :: Handle -> String -> Register -> Register -> Int -> String -> IO ()
emitRRIC out opcode r1 r2 value comment = hPutStr out $ "\t" ++ opcode ++ "\t" ++ show r1 ++ "," ++ show r2 ++ "," ++ show value ++ "\t\t; " ++ comment ++ "\n"

emitRRLC :: Handle -> String -> Register -> Register -> String -> String -> IO ()
emitRRLC out opcode r1 r2 label comment = hPutStr out $ "\t" ++ opcode ++ "\t" ++ show r1 ++ "," ++ show r2 ++ "," ++ label ++ "\t\t; " ++ comment ++ "\n"

emitRC :: Handle -> String -> Register -> String -> IO ()
emitRC out opcode r1 comment = hPutStr out $ "\t" ++ opcode ++ "\t" ++ show r1 ++ "\t\t\t; " ++ comment ++ "\n"

emitLC :: Handle -> String -> String -> String -> IO ()
emitLC out opcode label comment = hPutStr out $ "\t" ++ opcode ++ "\t" ++ label ++ "\t\t; " ++ comment ++ "\n"

emitLabel :: Handle -> String -> IO ()
emitLabel out label = hPutStr out $ label ++ ":\n"

emitImport :: Handle -> String -> IO ()
emitImport out label = hPutStr out $ "\t.import\t" ++ label ++ "\n"

emitExport :: Handle -> String -> IO ()
emitExport out label = hPutStr out $ "\t.export\t" ++ label ++ "\n"

emit :: Handle -> String -> IO ()
emit = hPutStrLn