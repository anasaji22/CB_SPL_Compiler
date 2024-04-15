module Token where

import Absyn.Position

data TokenClass = LBRACK | RBRACK | LPAREN | RPAREN | LCURL | RCURL
                | LT | LE | GT | GE | NE | EQ
                | PLUS | MINUS | STAR | SLASH
                | ASGN | COMMA | COLON | SEMIC
                | TYPE | PROC | ARRAY | OF | REF | VAR | IF | ELSE | WHILE
                | IDENT String
                | INTLIT Int
                deriving (Show, Eq)

data Token = Token Position TokenClass deriving (Show, Eq)

position :: Token -> Position
position (Token position _) = position
