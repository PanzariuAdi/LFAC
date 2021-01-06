%{
#include <stdio.h>
#include "y.tab.h"
%}
%option noyywrap
%%
"int"|"float"|"char"|"string"|"bool" {return TIP;}
"constant" {return CONST;}
"begin_progr" {return BGIN;}
"end_progr" {return END;}
"if" {return IF;}
"else" {return ELSE;}
"while" {return WHILE;}
"for" {return FOR;}
"<"|">"|"<="|">=" {return CMP_OP;}
"+"|"-"|"*"|"/" {return MATH_OP;}
"&&"|"||" {return BOOL_OP;}
"!" {return NEG_OP;}
@[_a-zA-Z][_a-zA-Z0-9]* {return ID;}
":=" {return ASSIGN;}
[0-9]+ {return NR;}
[ \t] ;
\n {yylineno++;}
. {return yytext[0];}

