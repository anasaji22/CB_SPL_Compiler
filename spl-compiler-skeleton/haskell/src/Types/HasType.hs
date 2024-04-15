module Types.HasType where

import Absyn.Node
import Data.IORef
import Types.Type

class (Node a) => HasType a where
  dataType :: a -> IORef (Maybe Type)

getType :: (HasType a) => a -> IO Type
getType a =
  readIORef (dataType a) >>= \case
    Just ty -> return ty
    Nothing -> fail "data type attribute of expression, variable or type is not initialized (did you forget to initialize it with `setType`?)"

setType :: (HasType a) => a -> Type -> IO ()
setType a ty = writeIORef (dataType a) (Just ty)