module Absyn.Statement where

import Absyn.Position
import Absyn.Expression
import Absyn.Node

data Statement = EmptyStatement Position
               | CompoundStatement Position [Statement]
               | AssignStatement Position Variable Expression
               | IfStatement Position Expression Statement Statement
               | WhileStatement Position Expression Statement
               | CallStatement Position String [Expression]

instance Show Statement where
    show :: Statement -> String
    show (EmptyStatement _) = "EmptyStatement" ++ format [] 
    show (CompoundStatement _ statements) = "CompoundStatement" ++ (format . map show) statements
    show (AssignStatement _ variable expression) = "AssignStatement" ++ format [show variable, show expression] 
    show (IfStatement _ condition thenPart elsePart) = "IfStatement" ++ format [show condition, show thenPart, show elsePart] 
    show (WhileStatement _ condition body) = "WhileStatement" ++ format [show condition, show body] 
    show (CallStatement _ procedureName arguments) = "CallStatement" ++ format [procedureName, "Arguments" ++ format (map show arguments)]

instance Node Statement where
    position :: Statement -> Position
    position (EmptyStatement position) = position
    position (CompoundStatement position _) = position
    position (AssignStatement position _ _) = position
    position (IfStatement position _ _ _) = position
    position (WhileStatement position _ _) = position
    position (CallStatement position _ _) = position
