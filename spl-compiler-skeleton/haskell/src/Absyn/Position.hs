module Absyn.Position where

data Position = Position { line :: Int, column :: Int } deriving Eq

instance Show Position where
    show :: Position -> String
    show (Position line column) = show line ++ ":" ++ show column

unknown :: Position
unknown = Position { line = -1, column = -1 }
