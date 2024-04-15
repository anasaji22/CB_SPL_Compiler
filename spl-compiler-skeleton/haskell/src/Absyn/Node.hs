module Absyn.Node where

import Absyn.Position

import Data.List(intercalate)

class Show a => Node a where
    position :: a -> Position

format :: [String] -> String
format []        = "()"
format arguments = "(\n" ++ indent 2 (intercalate ",\n" arguments) ++ ")" 

indent :: Int -> String -> String
indent n string = let indentation = replicate n ' ' in (intercalate "\n") $ (indentation ++) <$> lines string
