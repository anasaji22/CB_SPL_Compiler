module StackLayout where

import Data.Maybe (fromJust)
import Data.IORef (IORef, readIORef)

data StackLayout = StackLayout {argumentAreaSize :: IORef (Maybe Int), localVarAreaSize :: IORef(Maybe Int), outgoingAreaSize :: IORef(Maybe Int)}

frameSize :: StackLayout -> IO Int
frameSize (StackLayout _ localVarAreaSize outgoingAreaSize) = do
    undefined -- TODO: Implement

oldFramePointerOffset :: StackLayout -> IO Int
oldFramePointerOffset (StackLayout _ _ outgoingAreaSize) = do 
    undefined -- TODO: Implement

oldReturnAddressOffset :: StackLayout -> IO Int
oldReturnAddressOffset (StackLayout _ localVarAreaSize _) = do
    undefined -- TODO: Implement