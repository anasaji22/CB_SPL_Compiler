module TableInitializer where

import Data.IORef (newIORef)
import Table.SymbolTable
import Types.Type (byteSize, intType)
import VarAllocator (referenceByteSize)

initializeGlobalTable :: IO SymbolTable
initializeGlobalTable = do
  table <- newSymbolTable Nothing
  enterPredefinedTypes table
  enterPredefinedProcedures table

  return table

enterPredefinedTypes :: SymbolTable -> IO ()
enterPredefinedTypes table = enter table "int" $ TypeEntry intType

enterPredefinedProcedures :: SymbolTable -> IO ()
enterPredefinedProcedures table = do
  firstParameterOffset <- newIORef $ Just 0
  secondParameterOffset <- newIORef $ Just $ byteSize intType
  thirdParameterOffset <- newIORef $ Just $ 2 * byteSize intType
  fourthParameterOffset <- newIORef $ Just $ 3 * byteSize intType
  fifthParameterOffset <- newIORef $ Just $ 4 * byteSize intType

  predefinedProcedureEntry [ParameterType intType False firstParameterOffset] (byteSize intType) >>= enter table "printi"
  predefinedProcedureEntry [ParameterType intType False firstParameterOffset] (byteSize intType) >>= enter table "printc"
  predefinedProcedureEntry [ParameterType intType True firstParameterOffset] referenceByteSize >>= enter table "readi"
  predefinedProcedureEntry [ParameterType intType True firstParameterOffset] referenceByteSize >>= enter table "readc"
  predefinedProcedureEntry [] 0 >>= enter table "exit"
  predefinedProcedureEntry [ParameterType intType True firstParameterOffset] referenceByteSize >>= enter table "time"
  predefinedProcedureEntry [ParameterType intType False firstParameterOffset] (byteSize intType) >>= enter table "clearAll"
  predefinedProcedureEntry
    [ ParameterType intType False firstParameterOffset,
      ParameterType intType False secondParameterOffset,
      ParameterType intType False thirdParameterOffset
    ]
    (3 * byteSize intType)
    >>= enter table "setPixel"
  predefinedProcedureEntry
    [ ParameterType intType False firstParameterOffset,
      ParameterType intType False secondParameterOffset,
      ParameterType intType False thirdParameterOffset,
      ParameterType intType False fourthParameterOffset,
      ParameterType intType False fifthParameterOffset
    ]
    (5 * byteSize intType)
    >>= enter table "drawLine"
  predefinedProcedureEntry
    [ ParameterType intType False firstParameterOffset,
      ParameterType intType False secondParameterOffset,
      ParameterType intType False thirdParameterOffset,
      ParameterType intType False fourthParameterOffset
    ]
    (4 * byteSize intType)
    >>= enter table "drawCircle"
