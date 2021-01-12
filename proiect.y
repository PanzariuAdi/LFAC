%{
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#define UNDEFINED -1
#define INT 1
#define FLOAT 2
#define CHAR 3
#define STRING 4
#define BOOL 5
#define VAR 0
#define CON 1
#define NOFUNCTION 0
#define REGULARFUNCTION 1
#define METHOD 2
#define STBINDEX 50
#define MAXPARAMS 10
#define PLUS 0
#define MINUS 1
#define STAR 2
#define SLASH 3
#define LESS 4
#define LESSEQ 5
#define GR 6
#define GREQ 7
#define AND 8
#define OR 9

struct info
{
        int intval;
        char strval[100];
        float floatval;
        char charval;
        int type;
};

struct param
{
	char denumire[50];
	struct info inf;
};

struct symbol
{
        char denumire[50];
        int type;
        int varconst;
        int scope;
        int intVal;
        float floatVal;
        char charVal;
        char *stringVal;
	int assigned;
	int function;
	struct param params[MAXPARAMS];
	int vector;
	int *intVector;
	int index;
	char *charVector;
	char **stringVector;
	int *boolVector;
	float *floatVector;
}symbolTable[50];

struct var{
	int vector;
	int index;
	char *key;
};

void initList();
int ind(char* key);
void insertIntoList(char* name,char* type,int varconst,int scope);

int scope = 0;
int scopeStack[50];

void initScope();
void pushScope();
int getCurrentScope();
void popScope(int delete);
void assignVal(struct var *v,struct info *inf);
int assignVar(struct var *v1,struct var *v2 );
float convertToFloat(int part1,int part2);
float convertToFloatFromTxt(int part1,char *part2);
void initParamList(struct param *paramlist);
void insertParamsIntoParamList(struct param *paramlist,int sc);
struct param* insertParamsIntoParamListInfo(struct param *paramlist,struct info *inf);
void insertFunctionIntoTable(struct param *paramList,char *type,char* key);
int getVal(char *key);
int compute(struct info *inf1, int operator , struct info *inf2);
void eval(struct info *inf);
void checkForFunction(char* key,struct param *params);
void assignIDtoInfo(char* key,struct info *inf);
void insertVectorIntoList(char *tip,int index,char* key);
int getVectorVal(char * key, int i);
extern FILE* yyin;
extern char* yytext;
extern int yylineno;
%}
%union{int val;char *strval;char charval;struct info *inf;struct param *params;struct var *v}

%token BGIN END ASSIGN CONST IF ELSE WHILE NEG_OP FOR CLASS CLASS_SPEC BOOL_VAR STR_OP LB RB COMMA PRINT

%token <strval>ID
%token <strval>TIP
%token <val>NR
%token <strval>TEXT
%token <charval>CHR
%token <strval>FLT
%token <val>EVAL
%token <val>LESSOP
%token <val>LESSEQOP
%token <val>GROP
%token <val>GREQOP
%token <val>PLUSOP
%token <val>MINUSOP
%token <val>STAROP
%token <val>SLASHOP
%token <val>ANDOP
%token <val>OROP


%type <v>variable
%type <params>lista_param
%type <inf>value
%type <val>declaratieParam
%type <val>declaratiiParam
%type <inf>expression
%type <val>MATH_OP
%type <val>BOOL_OP
%type <val>CMP_OP
%type <val>operator
%type <inf>identifier
%type <inf>param
%start progr

%left '+' '-'
%left '*' '/'


%%
progr: declaratii bloc {printf("program corect sintactic\n");}
     ;

declaratii :
	   |  declaratie  ';' 
	   | declaratii declaratie ';' 
	   ;
declaratieTip : TIP ID 	{int k = getCurrentScope();insertIntoList($2,$1,VAR,k);}
           | TIP '[' NR ']' ID {insertVectorIntoList($1,$3,$5);}
           | CONST TIP ID ASSIGN NR
           ;
declaratiiParam : declaratieParam {$$ = $1;}
	      | declaratiiParam ',' declaratieParam {$1 = $3;$$ = $1;}
	      ;
declaratieParam : TIP ID {scope++;pushScope(); $$ = scope;insertIntoList($2,$1,VAR,scope),popScope(0);scope--;}

declaratieFunctii : TIP ID '(' declaratiiParam ')' {struct param paramList[MAXPARAMS];initParamList(paramList);insertParamsIntoParamList(paramList,$4);insertFunctionIntoTable(paramList,$1,$2);} lb list rb
		| TIP ID '(' ')' lb list rb {insertFunctionIntoTable(NULL,$1,$2);}
		;
lb : LB	{scope++; pushScope();}
   ;
rb : RB	{popScope(1);}
   ;

declaratie : declaratieTip
	   | declaratieFunctii
	   | PRINT {printList();}
           ;
lista_param : param {struct param *params = insertParamsIntoParamListInfo($$,$1); $$ = params;}
            | lista_param ','  param {struct param *params = insertParamsIntoParamListInfo($$,$3); $$ = params;}
            ;
param : expression
      | value
      | ID {struct info *in = (struct info*)malloc(sizeof(struct info));assignIDtoInfo($1,in) ;$$ = in;}
      | ID '[' NR ']'
      ;
/* bloc */
bloc : begin list end  
     ;
begin : BGIN {scope++;pushScope();}
     ;
end : END {popScope(1);}
    ;
     
/* lista instructiuni */
list :  statement ';' 
     | list statement ';'
     ;
/* instructiune */
statement: variable ASSIGN value	{assignVal($1,$3);}
	 | variable ASSIGN variable	{assignVar($1,$3);}
	 | variable ASSIGN expression {assignVal($1,$3);}
         | IF '(' expression  ')' lb list rb
         | IF '(' expression  ')' lb list rb ELSE lb list rb
         | WHILE '(' expression  ')' lb list rb
         | FOR '(' TIP ID ASSIGN value ';' expression ';' ID ASSIGN expression ')' lb list rb
         | CLASS ID lb class_items rb
         | STR_OP '(' ID ',' ID ')'
	 | declaratieTip
	 | ID'('lista_param')' {checkForFunction($1,$3);}
	 | ID '('')' {checkForFunction($1,NULL);}
	 | EVAL'(' expression ')' {eval($3);free($3);}
	 | PRINT {printList();}
         ;
variable : ID {struct var *v = (struct var*)malloc(sizeof(struct var));v->vector=0;v->index=0;v->key=$1;$$=v;}
         | ID '[' NR ']' {struct var *v = (struct var *)malloc(sizeof(struct var));v->vector=1;v->index=$3;v->key=$1;$$=v;}
        ;

value : NR	{struct info *in = (struct info*)malloc(sizeof(struct info));in->intval = $1;in->type = INT; $$ = in;}
      | TEXT 	{struct info *in = (struct info*)malloc(sizeof(struct info));strcpy(in->strval,$1);in->type = STRING; $$ = in;}
      | CHR {struct info *in = (struct info*)malloc(sizeof(struct info));in->charval = $1;in->type = CHAR; $$ = in;}
      | NR ',' NR	{struct info *in = (struct info*)malloc(sizeof(struct info));in->floatval = convertToFloat($1,$3);in->type = FLOAT; $$ = in;}
      | NR ',' FLT 	{struct info *in = (struct info*)malloc(sizeof(struct info));in->floatval = convertToFloatFromTxt($1,$3);in->type = FLOAT; $$ = in;}
      ;

class_item : CLASS_SPEC TIP ID '(' lista_param ')' '{' list '}'
          | CLASS_SPEC TIP ID '(' ')'	lb list rb
          | CLASS_SPEC TIP ID

class_items : class_item ';'
          | class_items class_item ';'


operator : MATH_OP {$$ = $1;}
          | BOOL_OP {$$ = $1;}
          | CMP_OP {$$ = $1;}
          ;
MATH_OP : PLUSOP {$$ = PLUS;}
	| MINUSOP {$$ = MINUS;}
	| STAROP {$$ = STAR;}
	| SLASHOP {$$ = SLASH;}
	;
BOOL_OP : ANDOP {$$ = AND;}
	| OROP {$$ = OR;}
	;
CMP_OP  : LESSOP {$$ = LESS;}
	| LESSEQOP {$$ = LESSEQ;}
	| GROP {$$ = GR;}
	| GREQOP {$$ = GREQ;}
	;

expression: identifier operator identifier {struct info *in = (struct info*)malloc(sizeof(struct info));in->intval = compute($1,$2,$3);$$ = in;free($1);free($3);}
	  | '(' expression ')' operator identifier {struct info *in = (struct info*)malloc(sizeof(struct info));in->intval = compute($2,$4,$5); $$=in ;free($2);free($5);}
	  | identifier operator '(' expression ')' {struct info *in = (struct info*)malloc(sizeof(struct info));in->intval = compute($1,$2,$4);$$ = in ; free($1);free($4);}
	  | '(' expression ')' operator '(' expression ')' {struct info *in = (struct info*)malloc(sizeof(struct info));in->intval = compute($2,$4,$6); $$ = in;free($2);free($6);}
	  | NEG_OP '(' expression ')'
	  | NEG_OP identifier
          ;
identifier : value {$$ = $1;}//NR {struct info *in = (struct info*)malloc(sizeof(struct info));in->intval = $1;in->type = INT; $$ = in;}
	   | ID {struct info *in = (struct info*)malloc(sizeof(struct info));in->intval = getVal($1);in->type = INT; $$ = in;}
	   | ID '[' NR ']' {struct info *in = (struct info*)malloc(sizeof(struct info));in->intval=getVectorVal($1,$3);in->type = INT; $$ = in;};
	   ;

%%

int findType(char *type)
{
int tip;
	if(!strcmp("int",type))
                                tip = INT;
                        else
                                if(!strcmp("float",type))
                                        tip = FLOAT;
                                else
                                        if(!strcmp("char",type))
                                                tip = CHAR;
                                        else
                                                if(!strcmp("string",type))
                                                        tip = STRING;
                                                else
                                                        if(!strcmp("bool",type))
                                                                 tip = BOOL;
                                                        else
                                                                tip = UNDEFINED;
return tip;
}
void initScope()
{
	for(int i = 0;i<=STBINDEX-1;i++)
		scopeStack[i] = -1;
	scopeStack[0] = scope;
}

void initParamList(struct param *paramlist)
{
	for(int i = 0;i<=MAXPARAMS-1;i++)
		bzero(paramlist[i].denumire,50),paramlist[i].inf.type = -1;
}
void initList()
{
	for(int i =0;i<=STBINDEX-1;i++)
		{
			bzero(symbolTable[i].denumire,50);
			symbolTable[i].type = -1;
			symbolTable[i].scope = -1;
			symbolTable[i].intVal = 0;
			symbolTable[i].floatVal = 0;
			symbolTable[i].charVal = '\0';
			symbolTable[i].stringVal = NULL;
			symbolTable[i].assigned = 0;
			symbolTable[i].function = NOFUNCTION;
			initParamList(symbolTable[i].params);
			symbolTable[i].vector = 0;
		}
}

int ind(char *key)
{
	int i = 0;
	while(symbolTable[i].type != -1 && i <STBINDEX)
	{
		if(strcmp(symbolTable->denumire,key) == 0)
			return -1;
		i++;
	}
	if(i>=STBINDEX)
		return -1;
	else
		return i;
}
int getVal(char* key)
{
	int ind = findInList(key);
	if(ind == -1)
	{
		printf("ERROR %s does not exist!\n",key),exit(1);
	}
	if(!symbolTable[ind].assigned)
		printf("ERROR %s NOT ASSIGNED\n",key),exit(1);
	/*
	if(symbolTable[ind].type != INT)
		printf("ERROR invalid item in expression!\n"),exit(1);*/
	return symbolTable[ind].intVal;
}
int getVectorVal(char * key, int i)
{
	int ind = findInList(key);
        if(ind == -1)
        {
                printf("ERROR %s does not exist!\n",key),exit(1);
        }
	return symbolTable[ind].intVector[i];
}
void insertIntoList(char *denumire,char* type,int varconst,int scope)
{
	int k = ind(denumire);
	int tip;
	if(k != -1)
		{
			strcpy(symbolTable[k].denumire,denumire); 
			if(!strcmp("int",type))
				tip = INT;
			else
				if(!strcmp("float",type))
                                	tip = FLOAT;
				else
					if(!strcmp("char",type))
                                		tip = CHAR;
					else
						if(!strcmp("string",type))
                                			tip = STRING;
						else
							if(!strcmp("bool",type))
                               					 tip = BOOL;
							else
								tip = UNDEFINED;
			symbolTable[k].varconst = varconst;
			symbolTable[k].type = tip;
			symbolTable[k].scope = scope; 
		}
	else
		printf("Eroare, multiple declarations of %s or symbolTable full!\n",denumire), exit(1);
}

void pushScope()
{
	int i = 0;
	while(scopeStack[i] != -1)
		i++;
	scopeStack[i] = scope;
}
int getCurrentScope()
{
	int i = 0;
	while(scopeStack[i+1] != -1)
		i++;
	return scopeStack[i];
}
int getCurrentScopeIndex()
{
	int i = 0;
        while(scopeStack[i+1] != -1)
                i++;
       	return i;

}

void rmv(int ind)
{
	if(symbolTable[ind].type == STRING && symbolTable[ind].assigned == 1)
		free(symbolTable[ind].stringVal);
	 bzero(symbolTable[ind].denumire,50);
         symbolTable[ind].type = -1;
         symbolTable[ind].scope = -1;
         symbolTable[ind].intVal = 0;
         symbolTable[ind].floatVal = 0;
         symbolTable[ind].charVal = '\0';
         symbolTable[ind].stringVal = NULL;
	symbolTable[ind].assigned = 0;

}
void popScope(int delete)
{
	int k = getCurrentScopeIndex();
	int s = getCurrentScope();
	if(delete)
		for(int i = 0 ;i <= STBINDEX-1;i++)
			if(symbolTable[i].scope == s && symbolTable[i].function == NOFUNCTION)
				rmv(i);
	scopeStack[k] = -1;
}
int findInList(char *key)
{
	for(int i = 0;i<=STBINDEX-1;i++)
		if(!strcmp(key,symbolTable[i].denumire))
			return i;
	return -1;
}
void removeQM(char* key)
{
	for(int i = 0;i<=strlen(key);i++)
		{
			key[i] = key[i+1];
		}
	key[strlen(key)-1] = '\n';
}
float convertToFloat(int part1,int part2)
{
	float k = part1;
	int temp = part2,count=0;
	while(temp!=0)
		count++,temp=temp/10;
	if(count !=0)
		k = k + (float)part2/pow(10,count);
	//printf("k = %f\n count = %d\n",k,part2/count)
	return k;

}
float convertToFloatFromTxt(int part1,char *part2)
{
	float k = part1,p2 = atoi(part2);
	k = k + p2/pow(10,strlen(part2));
	return k;
}
void assignVal(struct var *v,struct info *inf)
{
if(v->vector == 0)
{
	char key[100];
	strcpy(key,v->key);
	int ind = findInList(key);
	if(ind ==-1)
		{
			printf("Variable %s does not exist!\n",key);
			exit(1);
		}
	else
		{
			symbolTable[ind].assigned = 1;
			switch (symbolTable[ind].type){
				case INT:
					symbolTable[ind].intVal = inf->intval;
					break;
				case STRING:
					removeQM(inf->strval);
					symbolTable[ind].stringVal = (char*)malloc(100);
					strcpy(symbolTable[ind].stringVal,inf->strval);
					break;
				case CHAR:
					symbolTable[ind].charVal = inf->charval;
					break;
				case BOOL:
					if(inf->intval == 0)
						symbolTable[ind].intVal = 0;
					else
						symbolTable[ind].intVal = 1;
						
					break;
				case FLOAT:
					symbolTable[ind].floatVal = inf->floatval;
					break;
					
			}

		}
}
else
{
	int ind = findInList(v->key);
	if(ind ==-1)
                {
                        printf("Variable %s does not exist!\n",v->key);
                        exit(1);
                }
        else
                {
                        switch (symbolTable[ind].type){
                                case INT:
					//printf("writing %d to %s at key %d\n",inf->intval,v->key,v->index);
                                        symbolTable[ind].intVector[v->index] = inf->intval;
                                        break;
                                case STRING:
                                        removeQM(inf->strval);
                                        strcpy(symbolTable[ind].stringVector[v->index],inf->strval);
                                        break;
                                case CHAR:
                                        symbolTable[ind].charVector[v->index] = inf->charval;
                                        break;
                                case BOOL:
                                        if(inf->intval == 0)
                                                symbolTable[ind].boolVector[v->index] = 0;
                                        else
                                                symbolTable[ind].boolVector[v->index] = 1;

                                        break;
                                case FLOAT:
                                        symbolTable[ind].floatVector[v->index] = inf->floatval;
                                        break;

                        }

                }

}
free(inf);
}

int assignVar(struct var *v1,struct var *v2)
{
char key1[100],key2[100];
strcpy(key1,v1->key);
strcpy(key2,v2->key);
int ind1 = findInList(key1);
int ind2 = findInList(key2);
if(v1->vector == 0 && v2->vector == 0)
{
	if(ind1 == -1)
		printf("ERROR %s token does not exist!\n",key1),exit(1);
	if(ind2 == -1)
		printf("ERROR %s token does not exist!\n",key2),exit(1);
	if(symbolTable[ind1].type != symbolTable[ind2].type)
		printf("ERROR %s and %s have different types!\n",key1,key2),exit(1);
	if(symbolTable[ind2].assigned == 0)
		printf("ERROR %s is not assigned!\n",key2),exit(1);
	else
		{
			switch (symbolTable[ind1].type){
				case BOOL:
				case INT:
					symbolTable[ind1].intVal = symbolTable[ind2].intVal;
					break;
				case FLOAT:
					symbolTable[ind1].floatVal = symbolTable[ind2].floatVal;
					break;
				case CHAR:
					symbolTable[ind1].charVal = symbolTable[ind2].charVal;
					break;
				case STRING:
					if(symbolTable[ind1].assigned)
						strcpy(symbolTable[ind1].stringVal,symbolTable[ind2].stringVal);
					else
					{
						symbolTable[ind1].stringVal = (char*)malloc(100);
						strcpy(symbolTable[ind1].stringVal,symbolTable[ind2].stringVal);

					}
					break;
			}
			symbolTable[ind1].assigned = 1;
		}
	free(v1);
	free(v2);
	return 1;
}
if(v1->vector == 1 && v2->vector ==0)
{
        if(ind1 == -1)
                printf("ERROR %s token does not exist!\n",key1),exit(1);
        if(ind2 == -1)
                printf("ERROR %s token does not exist!\n",key2),exit(1);
        if(symbolTable[ind1].type != symbolTable[ind2].type)
                printf("ERROR %s and %s have different types!\n",key1,key2),exit(1);
        if(symbolTable[ind2].assigned == 0)
                printf("ERROR %s is not assigned!\n",key2),exit(1);
        else
                {
                        switch (symbolTable[ind1].type){
                                case BOOL:
					symbolTable[ind1].boolVector[v1->index] = symbolTable[ind2].intVal;
					break;
                                case INT:
                                        symbolTable[ind1].intVector[v1->index] = symbolTable[ind2].intVal;
                                        break;
                                case FLOAT:
                                        symbolTable[ind1].floatVector[v1->index] = symbolTable[ind2].floatVal;
                                        break;
                                case CHAR:
                                        symbolTable[ind1].charVector[v1->index] = symbolTable[ind2].charVal;
                                        break;
                                case STRING:
                                                strcpy(symbolTable[ind1].stringVector[v1->index],symbolTable[ind2].stringVal);
                                        break;
                        }
                }
        free(v1);
        free(v2);
        return 1;

}
if(v1->vector == 0 && v2->vector == 1)
{
        if(ind1 == -1)
                printf("ERROR %s token does not exist!\n",key1),exit(1);
        if(ind2 == -1)
                printf("ERROR %s token does not exist!\n",key2),exit(1);
        if(symbolTable[ind1].type != symbolTable[ind2].type)
                printf("ERROR %s and %s have different types!\n",key1,key2),exit(1);
        else
                {
                        switch (symbolTable[ind1].type){
                                case BOOL:
					symbolTable[ind1].intVal = symbolTable[ind2].boolVector[v2->index];
					break;
                                case INT:
                                        symbolTable[ind1].intVal = symbolTable[ind2].intVector[v2->index];
                                        break;
                                case FLOAT:
                                        symbolTable[ind1].floatVal = symbolTable[ind2].floatVector[v2->index];
                                        break;
                                case CHAR:
                                        symbolTable[ind1].charVal = symbolTable[ind2].charVector[v2->index];
                                        break;
                                case STRING:
                                        if(symbolTable[ind1].assigned)
                                                strcpy(symbolTable[ind1].stringVal,symbolTable[ind2].stringVector[v2->index]);
                                        else
                                        {
                                                symbolTable[ind1].stringVal = (char*)malloc(100);
                                                strcpy(symbolTable[ind1].stringVal,symbolTable[ind2].stringVector[v2->index]);

                                        }
                                        break;
                        }
                        symbolTable[ind1].assigned = 1;
                }
        free(v1);
        free(v2);
        return 1;
}
if(v1->vector == 1 && v2->vector ==1)
{
        if(ind1 == -1)
                printf("ERROR %s token does not exist!\n",key1),exit(1);
        if(ind2 == -1)
                printf("ERROR %s token does not exist!\n",key2),exit(1);
        if(symbolTable[ind1].type != symbolTable[ind2].type)
                printf("ERROR %s and %s have different types!\n",key1,key2),exit(1);
        else
                {
                        switch (symbolTable[ind1].type){
                                case BOOL:
					symbolTable[ind1].boolVector[v1->index] = symbolTable[ind2].boolVector[v2->index];
break;
                                case INT:
                                        symbolTable[ind1].intVector[v1->index] = symbolTable[ind2].intVector[v2->index];
                                        break;
                                case FLOAT:
                                        symbolTable[ind1].floatVector[v1->index] = symbolTable[ind2].floatVector[v2->index];
                                        break;
                                case CHAR:
                                        symbolTable[ind1].charVector[v1->index] = symbolTable[ind2].charVector[v2->index];
                                        break;
                                case STRING:
                                                strcpy(symbolTable[ind1].stringVector[v1->index],symbolTable[ind2].stringVector[v2->index]);
                                        break;
                        }
                }
        free(v1);
        free(v2);
        return 1;

}

}
void assignIDtoInfo(char* key,struct info *inf)
{
	int ind1 = findInList(key);
	if(ind1 == -1)
	{
		printf("ERROR %s does not exist!\n",key),exit(1);
 	}
	inf->type = symbolTable[ind1].type;
}

void insertParamsIntoParamList(struct param *paramlist,int sc)
{
	int ind = 0;
	for(int i = 0;i<=STBINDEX-1;i++)
		{
		if(symbolTable[i].scope == sc)
		{
			strcpy(paramlist[ind].denumire,symbolTable[i].denumire);
			paramlist[ind].inf.type = symbolTable[i].type;
			ind ++;
		}
		}
}
void insertFunctionIntoTable(struct param *paramList,char* type,char *key)
{
	int r = 0;
	int k = findInList(key);
	if(k != -1)
		printf("%s already exists!\n",key),exit(3);
	k = ind(key);
	strcpy(symbolTable[k].denumire,key);
	symbolTable[k].type = findType(type);
	symbolTable[k].function = REGULARFUNCTION;
	if(paramList!=NULL)
	while(paramList[r].denumire[0] != '\0' && r < MAXPARAMS)
	{
		strcpy(symbolTable[k].params[r].denumire,paramList[r].denumire);
		symbolTable[k].params[r].inf.type = paramList[r].inf.type;
		r++;
	}
	
	

}
int compute(struct info *inf1, int operator , struct info *inf2)
{
	switch (operator){
		case PLUS:
			return inf1->intval + inf2->intval;
			break;
		case MINUS: 
			return inf1->intval - inf2->intval;
			break;
		case STAR:
                        return inf1->intval * inf2->intval;
                        break;
		case SLASH:
                        return inf1->intval / inf2->intval;
                        break;
		case LESS:
                        return inf1->intval < inf2->intval;
                        break;

		case LESSEQ:
                        return inf1->intval <= inf2->intval;
                        break;

		case GR:
                        return inf1->intval > inf2->intval;
                        break;

		case GREQ:
                        return inf1->intval >= inf2->intval;
                        break;
		case AND:
                        return inf1->intval && inf2->intval;
                        break;

		case OR:
                        return inf1->intval || inf2->intval;
                        break;	
}
}

void eval(struct info *inf)
{
	printf("Valoarea este: %d\n",inf->intval);
}
struct param* insertParamsIntoParamListInfo(struct param *paramlist,struct info *inf)
{
	struct param *paramlist2 = (struct param*)malloc(sizeof(struct param) * MAXPARAMS);
	initParamList(paramlist2);
	if(paramlist[0].inf.type >=INT && paramlist[0].inf.type <=BOOL)
		{
			int k = 0;
			while(paramlist[k].inf.type != -1)
			{
				paramlist2[k].inf.type = paramlist[k].inf.type;
				k++;
			}
			paramlist2[k].inf.type = inf->type;
			free(paramlist);
		}
		else
			paramlist2[0].inf.type = inf->type;
	return paramlist2;
}
void checkForFunction(char* key,struct param *params)
{
	int in = findInList(key);	
	if(in == -1)
		printf("ERROR  %s there is no such function!\n",key),exit(1);
	if(params == NULL)
		{
			if(symbolTable[in].params[0].inf.type !=-1)
				printf("ERROR %s invalid number of parameters!\n",key),exit(2);
		}
	else
		{
			for(int i = 0;i<=MAXPARAMS;i++)
			{
				if(symbolTable[in].params[i].inf.type != params[i].inf.type)
					printf("ERROR invalid call of %s\n %d  %d\n",key,symbolTable[in].params[i].inf.type,params[i].inf.type),exit(1);		
			}		
		}
}
void insertVectorIntoList(char *tip,int index,char* key)
{
	int type = findType(tip);
	int k = findInList(key);
	if(k != -1)
		printf("ERROR %s already exists!\n",key),exit(1);
	k = ind(key);
	if(k == -1)
		printf("ERROR VARTABLE FULL!\n"),exit(1);
	symbolTable[k].type = type;
	symbolTable[k].index = index;
	symbolTable[k].vector=1;
	symbolTable[k].scope = getCurrentScope();
	strcpy(symbolTable[k].denumire,key);
	switch (type){
		case INT:
			symbolTable[k].intVector = (int*)malloc(index*sizeof(int));
			for(int i = 0;i<=index-1;i++)
				symbolTable[k].intVector[i] = 0;
			break;
		case BOOL:
			symbolTable[k].boolVector = (int*)malloc(index*sizeof(int));
			for(int i = 0;i<=index-1;i++)
                                symbolTable[k].boolVector[i] = 0;
			break;
		case CHAR:
			symbolTable[k].charVector = (char*)malloc(index*sizeof(char));
			bzero(symbolTable[k].charVector,index);
			break;
		case FLOAT:
			symbolTable[k].floatVector = (float*)malloc(index*sizeof(float));
			for(int i = 0;i<=index-1;i++)
                                symbolTable[k].floatVector[i] = 0;
			break;
		case STRING:
			symbolTable[k].stringVector = (char**)malloc(index*sizeof(char*));
			for(int i=0;i<=index-1;i++)
				symbolTable[k].stringVector[i] = (char*)malloc(100*sizeof(char)),bzero(symbolTable[k].stringVector[i],index);

			break;


	}
}
void printList()
{

remove("symbol_table.txt");
FILE *f = fopen("symbol_table.txt","a");
for(int i = 0;i<=STBINDEX-1;i++)
{
        if(symbolTable[i].type != -1 && symbolTable[i].function == NOFUNCTION)
	{
                fprintf(f,"denumire: %s\ntip: %d\nscope: %d\nassigned: %d\n",symbolTable[i].denumire,symbolTable[i].type,symbolTable[i].scope,symbolTable[i].assigned);
		if(symbolTable[i].assigned == 1 && symbolTable[i].vector==0)
			{
				switch (symbolTable[i].type){
					case BOOL:
					case INT:
						fprintf(f,"value: %d\n",symbolTable[i].intVal);
						break;
					case STRING:
						fprintf(f,"value: %s\n",symbolTable[i].stringVal);
						break;
					case CHAR:
						fprintf(f,"value: %c\n",symbolTable[i].charVal);
						break;
					case FLOAT:
						fprintf(f,"value: %f\n",symbolTable[i].floatVal);
						break;
				}
			}
		if(symbolTable[i].vector == 1)
		{
			fprintf(f,"index: %d\n",symbolTable[i].index);
			fprintf(f,"vector values: ");
			for(int j = 0;j<=symbolTable[i].index-1;j++)
			{
				switch (symbolTable[i].type){
					case INT:
						fprintf(f,"%d.%d   ",j,symbolTable[i].intVector[j]);
					break;
					 case BOOL:
                                                fprintf(f,"%d.%d   ",j,symbolTable[i].boolVector[j]);
                                        break;
					 case CHAR:
                                                fprintf(f,"%d.%s   ",j,symbolTable[i].charVector);
                                        break;
					 case FLOAT:
                                                fprintf(f,"%d.%f   ",j,symbolTable[i].floatVector[j]);
                                        break;
					 case STRING:
                                                fprintf(f,"%d.%s   ",j,symbolTable[i].stringVector[j]);
                                        break;
					default:
						printf("unknown\n");
						break;
				}
			}
		}
		fprintf(f,"\n\n");
	}
		
}
fprintf(f,"\n\nFUNCTII:\n\n");
for(int i = 0;i<=STBINDEX-1;i++)
	if(symbolTable[i].function == REGULARFUNCTION)
		{
			fprintf(f,"denumire: %s\n",symbolTable[i].denumire);
			int k = 0;
			while(symbolTable[i].params[k].denumire[0] != '\0' && k<MAXPARAMS)
			{
				fprintf(f,"param: %s     type: %d\n",symbolTable[i].params[k].denumire,symbolTable[i].params[k].inf.type);
				k++;
			}
			fprintf(f,"retType: %d\n\n",symbolTable[i].type);
		}

}
void yyerror(char * s){
printf("eroare: %s la linia:%d\n",s,yylineno);
}

int main(int argc, char** argv){
initScope();
initList();
yyin=fopen(argv[1],"r");
yyparse();

} 
