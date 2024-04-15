module Absyn.Program where

import Absyn.Position
import Absyn.Node
import Absyn.Declaration

data Program = Program Position [GlobalDeclaration]

instance Show Program where
    show :: Program -> String
    show (Program _ declarations) = "Program" ++ format (map show declarations)

instance Node Program where
    position :: Program -> Position
    position (Program position _) = position
