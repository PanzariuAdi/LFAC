%{
#include <stdio.h>
#include "y.tab.h"
%}
%option noyywrap
%%
"int"|"float"|"char"|"string"|"bool"|"undefined" {yylval.strval = strdup(yytext); return TIP;}
"constant" {return CONST;}
"begin_progr" {return BGIN;}
"end_progr" {return END;}
"if" {return IF;}
"else" {return ELSE;}
"while" {return WHILE;}
"for" {return FOR;}
"class" {return CLASS;}
"private"|"protected"|"public" {return CLASS_SPEC;}
"true"|"false" {return BOOL_VAR;}
"<" {return LESSOP;}
"<=" {return LESSEQOP;}
">" {return GROP;}
">=" {return GREQOP;}
"+" {return PLUSOP;}
"-" {return MINUSOP;}
"*" {return STAROP;}
"/" {return SLASHOP;}
"&&" {return ANDOP;}
"||" {return OROP;}
"!" {return NEG_OP;}
"strcpy"|"strcat" {return STR_OP;}
@[_a-zA-Z][_a-zA-Z0-9]* {yylval.strval = strdup(yytext); return ID;}
\"[ !-~]+\" {yylval.strval = strdup(yytext); return TEXT;}
'[ !-~]' {yylval.charval = yytext[1];return CHR;}
":=" {return ASSIGN;}
[1-9]+[0-9]* {yylval.val = atoi(yytext); return NR; }
0+[0-9]* {yylval.strval = strdup(yytext);return FLT;}
0 {yylval.val = atoi(yytext);return NR;}
"{" {return LB;}
"}" {return RB;}
EVAL {return EVAL;}
PRINT {return PRINT;}
[ \t] ;
\n {yylineno++;}
. {return yytext[0];}
