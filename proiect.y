%{
#include <stdio.h>
extern FILE* yyin;
extern char* yytext;
extern int yylineno;
%}
%token ID TIP BGIN END ASSIGN NR CONST IF ELSE WHILE CMP_OP MATH_OP BOOL_OP NEG_OP FOR CLASS CLASS_SPEC

%left '+' '-'
%left '*' '/'

%start progr
%%
progr: declaratii bloc {printf("program corect sintactic\n");}
     ;

declaratii :  declaratie ';'
	   | declaratii declaratie ';'
	   ;
declaratie : TIP ID 
           | TIP ID '(' lista_param ')'
           | TIP ID '(' ')'
           | TIP '[' NR ']' ID
           | CONST TIP ID ASSIGN NR
           ;
lista_param : param
            | lista_param ','  param 
            ;

param : TIP ID
      ; 
      
/* bloc */
bloc : BGIN list END  
     ;
     
/* lista instructiuni */
list :  statement ';' 
     | list statement ';'
     ;

/* instructiune */
statement: ID ASSIGN ID
         | ID ASSIGN NR  		 
         | ID '(' lista_apel ')'
         | IF '(' condition  ')' '{' list '}'
         | IF '(' condition  ')' '{' list '}' ELSE '{' list '}'
         | WHILE '(' condition  ')' '{' list '}'
         | FOR '(' TIP ID ASSIGN value ';' condition ';' expression ')' '{' list '}'
         | CLASS ID '{' class_items '}'
         ;

value : ID
      | NR
      ;

class_item : CLASS_SPEC TIP ID '(' lista_apel ')' '{' list '}'
          | CLASS_SPEC TIP ID '(' ')' '{' list '}'
          | CLASS_SPEC TIP ID

class_items : class_item ';'
          | class_items class_item ';'

condition : NR CMP_OP NR
          | NR CMP_OP ID
          | ID CMP_OP NR
          | ID CMP_OP ID
          | NEG_OP '(' condition ')'
          ;

conditions : condition BOOL_OP conditions
          | condition
          ; 


lista_apel : NR
          | lista_apel ',' NR
          ;

operator : MATH_OP
          | BOOL_OP
          | CMP_OP
          | ASSIGN
          ;

expression : value
          | value operator expression
          ;

aexp : aexp '+' aexp { $$ = $1 + $3; }
     | aexp '-' aexp { $$ = $1 - $3; }
     | aexp '*' aexp { $$ = $1 * $3; }
     | aexp '/' aexp { $$ = $1 / $3; }
     ;

%%
void yyerror(char * s){
printf("eroare: %s la linia:%d\n",s,yylineno);
}

int main(int argc, char** argv){
yyin=fopen(argv[1],"r");
yyparse();
} 