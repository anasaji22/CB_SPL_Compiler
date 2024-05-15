package de.thm.mni.compilerbau.phases._01_scanner;

import de.thm.mni.compilerbau.utils.SplError;
import de.thm.mni.compilerbau.phases._02_03_parser.Sym;
import de.thm.mni.compilerbau.absyn.Position;
import de.thm.mni.compilerbau.table.Identifier;
import de.thm.mni.compilerbau.CommandLineOptions;
import java_cup.runtime.*;

%%


%class Scanner
%public
%line
%column
%cup
%eofval{
    return new java_cup.runtime.Symbol(Sym.EOF, yyline + 1, yycolumn + 1);   //This needs to be specified when using a custom sym class name
%eofval}

%{
    public CommandLineOptions options = null;
  
    private Symbol symbol(int type) {
      return new Symbol(type, yyline + 1, yycolumn + 1);
    }

    private Symbol symbol(int type, Object value) {
      return new Symbol(type, yyline + 1, yycolumn + 1, value);
    }
%}

%%

// TODO (assignment 1): The regular expressions for all tokens need to be defined here.

if { return symbol(Sym.IF);}
else { return symbol(Sym.ELSE);}
array { return symbol(Sym.ARRAY);}
of { return symbol(Sym.OF);}
proc { return symbol(Sym.PROC);}
ref { return symbol(Sym.REF);}
type { return symbol(Sym.TYPE);}
while { return symbol(Sym.WHILE);}
var { return symbol(Sym.VAR);}
[a-zA-Z_][a-zA-Z0-9_]* { return symbol(Sym.IDENT, new Identifier(yytext()));}
[0-9][0-9]* { return symbol(Sym.INTLIT, Integer.parseInt(yytext()));}
0x[0-9a-fA-F]+ { return symbol(Sym.INTLIT, Integer.parseInt(yytext().substring(2), 16));}
\( { return symbol(Sym.LPAREN);}
\) { return symbol(Sym.RPAREN);}
\{ { return symbol(Sym.LCURL);}
\} { return symbol(Sym.RCURL);}
\[ { return symbol(Sym.LBRACK);}
\] { return symbol(Sym.RBRACK);}
\+ { return symbol(Sym.PLUS);}
\- { return symbol(Sym.MINUS);}
\* { return symbol(Sym.STAR);}
\/ { return symbol(Sym.SLASH);}
"//".* {}
\s {}
\= { return symbol(Sym.EQ);}
\# { return symbol(Sym.NE);}
\< { return symbol(Sym.LT);}
\> { return symbol(Sym.GT);}
\<= { return symbol(Sym.NE);}
\>= { return symbol(Sym.GE);}
\, { return symbol(Sym.COMMA);}
\; { return symbol(Sym.SEMIC);}
\: { return symbol(Sym.COLON);}
\:= { return symbol(Sym.ASGN);}
\# { return symbol(Sym.NE);}
'\\n' { return symbol(Sym.INTLIT, 10); }
'.' { return symbol(Sym.INTLIT, (int) yytext().charAt(1)); }
[^]		{throw SplError.LexicalError(new Position(yyline + 1, yycolumn + 1), yytext().charAt(0));}
