module Types.Type where

import Data.IORef
import GHC.IO (unsafePerformIO)

data Type
  = PrimitiveType Int String
  | ArrayType Type Int Int

instance Show Type where
  show :: Type -> String
  show (PrimitiveType _ printName) = printName
  show (ArrayType baseType arraySize _) = "array [" ++ show arraySize ++ "] of " ++ show baseType

idCounter :: IORef Int
{-# NOINLINE idCounter #-}
idCounter = unsafePerformIO (newIORef 0)

nextUniqueId :: IO Int
nextUniqueId = do
  current <- readIORef idCounter
  writeIORef idCounter (current + 1)
  return current

byteSize :: Type -> Int
byteSize (PrimitiveType b _) = b
byteSize (ArrayType baseType arraySize _) = byteSize baseType * arraySize

intType :: Type
intType = PrimitiveType 4 "int"

boolType :: Type
boolType = PrimitiveType 4 "boolean"

isArrayType :: Type -> Bool
isArrayType (ArrayType {}) = True
isArrayType _ = False