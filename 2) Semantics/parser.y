%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symbol.h"




extern int yylex();
extern int yylineno;
extern char *yytext;

extern Data data;

int yyerror(const char *str);

%}

%union {
  char* string;
  struct Tree* tree;
}


%token  BOOL CHAR VOID INT STRING INTP CHARP REAL REALP
%token  IF ELSE WHILE RETURN
%token  AND_OP EQ_OP GE_OP LE_OP NE_OP OR_OP
%token  NUL VAR ':' FUN 
%token <string> CONST_STRING ID MAINID CONST_INT CONST_REAL CONST_CHAR TRUE FALSE

%type <tree> S FUNC FUNCS PAR_LIST BLOCK RETURN_EXP BLOCK_BODY PAR_DEC STRING_PAR ALL_PARS
%type <tree> DECLERATIONS STATMENTS DECLARE STATMENT IDS_LIST STRING_IDS_LIST
%type <tree>  EXP TYPE_NOSTRING TYPES IDN MAIN PARS STRING_TYPE
%type <tree> STRING_INDEX FUNC_CALL CALL_ID_LIST POINTER_DER POINTER_REF UN_MINUS
%type <tree> LHS ASSIGMENT WHILE_STATMENT STATMENT_NOT_IF IF_STATMENT
%type <tree> MATCHED UNMATCHED 

%right '='
%left OR_OP
%left AND_OP
%left EQ_OP NE_OP
%left '>' '<' GE_OP LE_OP
%left '+' '-'
%left '*' '/'
%right '&'
%right '^'

%%
S:FUNCS { data.ast=$1;test();}
 ;
FUNCS:FUNC {$$=$1;}
     | FUNCS FUNC {$$=make2Tree("", Other,$1,$2);}
     ;
FUNC: FUN IDN  PAR_LIST RETURN TYPES BLOCK  {CheckHaveReturn($6,$2->string);$$=make4Tree("FUNC",Func,$5,$2,$3,$6);}
     | VOID IDN PAR_LIST  BLOCK  {$$=make4Tree("FUNC",Func,makeLeaf("VOID",Void),$2,$3,$4);}
     | VOID MAIN PAR_LIST BLOCK  {$$=make4Tree("FUNC",Func,makeLeaf("VOID",Void),$2,$3,$4);};


TYPE_NOSTRING: INT {$$=makeLeaf("INT",Int);}
     | CHAR {$$=makeLeaf("CHAR",Char);}
     | BOOL {$$=makeLeaf("BOOL",Bool);}
     | INTP {$$=makeLeaf("INTP",Intp);}
     | CHARP {$$=makeLeaf("CHARP",Charp);}
     | REAL {$$=makeLeaf("REAL",Real);}
     | REALP {$$=makeLeaf("REALP",Realp);}
     ;
TYPES:TYPE_NOSTRING {$$=$1;}
|STRING_TYPE {$$=$1;};
MAIN: MAINID {$$=makeLeaf($1,Id);$$->lineno=yylineno;}
   ;
IDN: ID {$$=makeLeaf($1,Id);$$->lineno=yylineno;}
   ;

STRING_TYPE: STRING {$$=makeLeaf("STRING",String);}
;

PAR_LIST: '(' ')' {$$=makeLeaf("PARAMETERS",Empty);}
        | '(' PARS ')' {$$=$2;}
        ;
        
PARS: ALL_PARS {$$=$1;}
      | PARS ';' ALL_PARS {$$=make2Tree("",Other,$1,$3);}
      ;

ALL_PARS:PAR_DEC {$$=$1;} | STRING_PAR {$$=$1;};

PAR_DEC: IDN ':' TYPE_NOSTRING{$$=make2Tree("",Par,$3,$1);}
	| IDN ',' PAR_DEC {$$=make2Tree("",Par,$3,$1);}
        ;
        
STRING_PAR:  IDN ':' STRING_TYPE '[' EXP ']'{$$=make3Tree("",Par,$3,$1,$5);}
	| IDN ',' STRING_PAR {$$=make2Tree("",Par,$3,$1);}
        ;       
        
BLOCK: '{' '}' {$$=makeLeaf("BLOCK",Empty);}
     | '{' BLOCK_BODY '}' {$$=make1Tree("BLOCK",Block,$2);}
     ;
RETURN_EXP: RETURN EXP ';' {$$=make1Tree("RETURN",Return,$2);}
          | RETURN ';' {$$=makeLeaf("RETURN",Return);}
           ;

BLOCK_BODY: DECLERATIONS STATMENTS {$$=make2Tree("",Other,$1,$2);}
           | DECLERATIONS {$$=$1;}
           | STATMENTS {$$=$1;}
          ;

DECLERATIONS: DECLARE {$$=$1;}
             | DECLERATIONS DECLARE {$$=make2Tree("",Other,$1,$2);}
             ;

DECLARE: VAR IDS_LIST ';' {$$=make1Tree("DECLARE",Decs,$2);}
        | FUNC {$$=$1;}
        |VAR STRING_IDS_LIST '[' EXP ']' ';'{PutExp($2,$4);$$=make1Tree("DECLARE",Decs,$2);}
        ;

STRING_IDS_LIST:  IDN ':' STRING_TYPE{$$=make2Tree("",Decs,$3,make2Tree("[]",Stringindex,$1,makeLeaf("as",0)));}
                | IDN ',' STRING_IDS_LIST  {$$=make2Tree("",Decs,$3,make2Tree("[]",Stringindex,$1,makeLeaf("as",0)));}
                ;

STRING_INDEX: IDN '[' EXP ']' {$$=make2Tree("[]",Stringindex,$1,$3);};


IDS_LIST: IDN ':' TYPE_NOSTRING {$$=make2Tree("",Other,$3,$1);}
         | IDN ',' IDS_LIST {$$=make2Tree("",Other,$3,$1);}
         ;
        

STATMENTS: STATMENT {$$=$1;}
          | STATMENTS STATMENT {$$=make2Tree("",Other,$1,$2);}
          ;

STATMENT: STATMENT_NOT_IF {$$=$1;}
        | IF_STATMENT {$$=$1;}
        ;

STATMENT_NOT_IF:ASSIGMENT ';' {$$=$1;}
        | FUNC_CALL ';' {$$=$1;}
        | WHILE_STATMENT {$$=$1;}
        | BLOCK {$$=$1;}
        | RETURN_EXP {$$=$1;}
        ;

IF_STATMENT: MATCHED {$$=$1;}
          | UNMATCHED {$$=$1;}
;

MATCHED: IF '(' EXP ')' MATCHED ELSE MATCHED {$$=make3Tree("IF-ELSE",If,$3,$5,$7);}
       | STATMENT_NOT_IF {$$=$1;}
       ;

UNMATCHED: IF '(' EXP ')' MATCHED {$$=make2Tree("IF",If,$3,$5);}
         | IF '(' EXP ')' UNMATCHED {$$=make2Tree("IF",If,$3,$5);}
         | IF '(' EXP ')' MATCHED ELSE UNMATCHED {$$=make3Tree("IF-ELSE",If,$3,$5,$7);}
         ;

ASSIGMENT:LHS '=' EXP  {$$=make2Tree("=",Asign,$1,$3);}
;

WHILE_STATMENT: WHILE '(' EXP ')' STATMENT {$$=make2Tree("WHILE",While,$3,$5);}
;

LHS: IDN {$$=$1;}
   | POINTER_DER {$$=$1;}
   | STRING_INDEX {$$=$1;}
   ;

EXP: IDN {$$=$1;}
   | CONST_INT {$$=makeLeaf($1,Int);$$->lineno=yylineno;}
   | CONST_CHAR {$$=makeLeaf($1,Char);$$->lineno=yylineno;}
   | CONST_STRING {$$=makeLeaf($1,String);$$->lineno=yylineno;}
   | CONST_REAL {$$=makeLeaf($1,Real);$$->lineno=yylineno;}
   | NUL {$$=makeLeaf("null",Null);}
   | TRUE {$$=makeLeaf("TRUE",Bool);}
   | FALSE {$$=makeLeaf("FALSE",Bool);}
   | STRING_INDEX {$$=$1;}
   | '|' IDN '|' {$$=make1Tree("||",Abs,$2);}
   | EXP AND_OP EXP {$$=make2Tree("AND",Ebool,$1,$3);}
   |EXP OR_OP EXP {$$=make2Tree("OR",Ebool,$1,$3);}
   |EXP EQ_OP EXP {$$=make2Tree("==",Eall2bool,$1,$3);}
   |EXP LE_OP EXP {$$=make2Tree("<=",Eint2bool,$1,$3);}
   |EXP GE_OP EXP {$$=make2Tree(">=",Eint2bool,$1,$3);}
   |EXP '>' EXP {$$=make2Tree(">",Eint2bool,$1,$3);}
   |EXP '<' EXP {$$=make2Tree("<",Eint2bool,$1,$3);}
   |EXP NE_OP EXP {$$=make2Tree("!=",Eall2bool,$1,$3);}
   |EXP '+' EXP {$$=make2Tree("+",Eint,$1,$3);}
   |EXP '-' EXP {$$=make2Tree("-",Eint,$1,$3);}
   |EXP '*' EXP {$$=make2Tree("*",Eint,$1,$3);}
   |EXP '/' EXP {$$=make2Tree("/",Eint,$1,$3);}
   | '(' EXP ')' {$$=$2;}
   | POINTER_DER {$$=$1;}
   | POINTER_REF {$$=$1;}
   | FUNC_CALL {$$=$1;}
   | '!' EXP {$$=make1Tree("!",Ebool,$2);}
   | UN_MINUS {$$=$1;}
   ;

UN_MINUS: '-' EXP {$$=make1Tree("-",$2->type,$2);}
;
FUNC_CALL: IDN '(' ')' {$$=make1Tree("FUNC CALL",Funccall,$1);}
         | IDN '(' CALL_ID_LIST ')'  {$$=make2Tree("FUNC CALL",Funccall,$1,$3);}
         ;

CALL_ID_LIST: EXP {$$=make1Tree("",Other,$1);}
         | CALL_ID_LIST ',' EXP {$$=make2Tree("",Other,$1,$3);}
         ;
POINTER_DER: '^' IDN {$$=make1Tree("^",Dref,$2);}
| '^' '(' IDN '+' EXP ')' {$$=make1Tree("^",Dref,$3);if($5->type != Eint && $5->type != Int ){printf("pointer derefernce error , line number: %d\n",yylineno);exit(1);}}
           | '^' '(' IDN '-' EXP ')' {$$=make1Tree("^",Dref,$3);if($5->type != Eint && $5->type != Int){printf("pointer derefernce error , line number: %d\n",yylineno);exit(1);}};

POINTER_REF:'&' IDN {$$=make1Tree("&",Ref,$2);}
           |'&' STRING_INDEX {$$=make1Tree("&",Ref,$2);}

%%
int yyerror(const char *str)
{
	fprintf(stderr, "%s error , line number: %d\n", str,yylineno);
	fprintf(stderr, "parser caused by: '%s'\n",yytext);
}

int main() {
  yyparse();
  return 0;
}
Tree* make4Tree(char* str,int type, Tree* n1, Tree* n2, Tree* n3, Tree* n4)
{
   Tree *result= (Tree*) malloc (sizeof(Tree));
   result->n1=n1;
    result->n2=n2;
    result->n3=n3;
     result->n4=n4;
     result->type=type;
    result->string=str;
   return result;
}
Tree* make3Tree(char* str,int type, Tree* n1, Tree* n2, Tree* n3)
{
   return make4Tree(str,type,n1,n2,n3,NULL);
}
Tree* make2Tree(char* str,int type, Tree* n1, Tree* n2)
{
    return make4Tree(str,type,n1,n2,NULL,NULL);
}
Tree* make1Tree(char* str,int type, Tree* n1)
{
   return make4Tree(str,type,n1,NULL,NULL,NULL);
}
Tree* makeLeaf(char* str, int type)
{
   return make4Tree(str,type,NULL,NULL,NULL,NULL);
}

void printTree(Tree* t,int offset)
{ 
  int i=0,j;
  if(t->n1!=NULL)i++;
  if(t->n2!=NULL)i++;

  if (i==0)
    printf("%s ",t->string);
  else if (i==1)
  {
       printf("(%s ",t->string);
       printTree(t->n1,offset);
       printf(")");        
  }
  else
  {
      printf("\n");
      for(j=0;j<offset;j++)
      printf("|  ");
      printf("(%s ",t->string);
      printTree(t->n1,offset+1);
      printTree(t->n2,offset+1);
      if(t->n3!=NULL)
          printTree(t->n3,offset+1);
      if(t->n4!=NULL)
          printTree(t->n4,offset+1);
      printf(")\n");
       for(j=0;j<offset-1;j++)
      printf("|  ");
    }
}
void deleteTree(Tree* t)
{
  int i;

  deleteTree(t->n1);
  deleteTree(t->n2);
  deleteTree(t->n3);
  deleteTree(t->n4);
   free(t);
}

void PutExp(Tree* decs ,Tree* exp)
{
if(decs->type == Stringindex)
    decs->n2=exp;

if(decs->n1 !=NULL)
    PutExp(decs->n1,exp);
    
if(decs->n2 !=NULL)
    PutExp(decs->n2,exp);    
}
void HaveReturn(Tree* decs ,int* have)
{
if(decs->type == Return)
    (*have)=1;

if(decs->n1 !=NULL)
    HaveReturn(decs->n1,have);
    
if(decs->n2 !=NULL)
    HaveReturn(decs->n2,have);    
}
void CheckHaveReturn(Tree* decs,char* FuncName)
{
 int a=0;
 int* have=&a;
 HaveReturn(decs,have);
 if(a==0)
 {
    printf("error:Function %s should have a return statement \n",FuncName);
    exit(1);
 }
 
 
}




