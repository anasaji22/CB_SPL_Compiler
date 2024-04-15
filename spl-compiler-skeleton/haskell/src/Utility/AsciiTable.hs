module Utility.AsciiTable where

data Alignment = LEFT | CENTER deriving (Show)

data CellType = TITLE | CONTENT deriving (Show)

data Cell = Cell CellType Alignment String String deriving (Show)

newtype AsciiTable = AsciiTable [Cell] deriving (Show)

emptyAsciiTable :: AsciiTable
emptyAsciiTable = AsciiTable []

addCell :: AsciiTable -> Cell -> AsciiTable
addCell (AsciiTable cells) cell = AsciiTable $ cells ++ [cell]