%{
#include<stdio.h>
#include "lex.yy.c"
#include<string.h>
#include<stdlib.h>

extern int yylex();
extern int yylineno;
extern char *yytext;
int yyerror(const char *str);

typedef struct node
{
char* token;
struct node* n1;
struct node* n2;
struct node* n3;
struct node* n4;
}node;

node *MakeLeaf(char *token);
node *MakeTree1(char *token, node *n1);
node *MakeTree2(char *token, node *n1, node *n2);
node *MakeTree3(char *token, node *n1, node *n2 ,node *n3);
node *MakeTree4(char *token, node *n1, node *n2 ,node *n3,node *n4);

void printtree(node *tree,int i);



%}
%union { 
char* string;
struct node* Tree;
}
 
%token MAIN BOOL CHAR INT REAL STRING INTP CHARP REALP IF ELSE WHILE VAR FUNC PROC RETURN NULLV STRINGARR
%token AND '/' '=' EQ G GE L LE '-' '!' NEQ OR '+' '*' '&' '^' ',' ':' ';' '|' '{' '}' '(' ')' '[' ']' BOOLV
%token <string> INTV CHARV STRINGV REALV ID  
%type <Tree> EXP PTR_DEC ASSIGNMENT FUNC_CALL EXPS RETRUN_VAL STRING_IND  TYPE VAR_DECL STRING_DECL PAR_LIST
%type <Tree> STATEMENT_WITHOUT_IF DECLRATION DECLRATIONS STATEMENT STATEMENTS  FUNCS CODE US MS
%type <Tree> IF_STATEMENT WHILE_STATEMENT FUNCTIONS FUNCTION PROCED MAIN_FUNC RETURN_BLOCK BLOCK BLOCK_BODY 
%type <string> OPR VAL IDS

%right '='
%left OR
%left AND
%left EQ NEQ
%left G L GE LE
%left '+' '-'
%left '*' '/'
%right '&'
%right '^'


%%
S: CODE {printtree($1,0);} ;

CODE:/*DONE*/
FUNCS MAIN_FUNC {$$=MakeTree2("CODE",$1,$2);} 
| MAIN_FUNC  {$$=MakeTree1("CODE",$1);} ;

FUNCS :/*DONE*/
 FUNCS FUNCTIONS {$$=MakeTree2("",$1,$2);}| FUNCTIONS {$$=$1;};

TYPE : /*DONE*/
BOOL {$$=MakeLeaf("BOOL");}
| CHAR {$$=MakeLeaf("CHAR");}
| INT {$$=MakeLeaf("INT");}
| REAL {$$=MakeLeaf("REAL");}
| INTP {$$=MakeLeaf("INT*");}
| CHARP {$$=MakeLeaf("CHAR*");}
| REALP {$$=MakeLeaf("REAL*");}
| STRING {$$=MakeLeaf("STRING");} ;

IDS :/*DONE*/
 IDS ',' ID {$$=strcat($$,strcat($3," "));}  
|ID {strcat($1," ");} ;

VAR_DECL : /*DONE*/
VAR IDS ':' TYPE ';' {$$=MakeTree1($4->token,MakeLeaf($2));} ;

STRING_DECL : /*DONE*/
VAR IDS ':' STRINGARR ';' {$$=MakeTree1("STRING",MakeLeaf($2));} ;

EXP : /*DONE*/
ID {$$=MakeLeaf($1);}| '(' EXP ')'{$$=$2;} | '!' EXP {$$=MakeTree1("!",$2);}
| EXP OPR EXP {$$=MakeTree2($2,$1,$3);} 
| '^' '(' EXP ')' {$$=MakeTree1("^",$3);}
|'^' ID  {$$=MakeTree1("^",MakeLeaf($2));}
| VAL {$$=MakeLeaf($1);}
| '-' EXP {$$=MakeTree1("-",$2);}
|'+' EXP {$$=MakeTree1("+",$2);}
| '|' ID '|' {$$=MakeTree1("Abs",MakeLeaf($2));}
|STRING_IND {$$=$1;}
| FUNC_CALL {$$=$1;}; 
     
VAL :/*DONE*/
INTV {$$=$1;}
|CHARV {$$=$1;}
|REALV {$$=$1;}
|STRINGV {$$=$1;}
|BOOLV {$$="BOOL";}
|NULLV{$$="NULL";} ;

STRING_IND :/*DONE*/
 ID '[' EXP ']' {$$=MakeTree2("[]",MakeLeaf($1),$3);};

OPR :/*DONE*/ 
'+' {$$="+";}
|'-' {$$="-";} 
|'*' {$$="*";}
|'/' {$$="/";}
|OR {$$="OR";}
|AND {$$="AND";}
|EQ {$$="==";}
|NEQ {$$="!=";}
|G {$$=">";}
|GE {$$=">=";}
|L{$$="<";} 
|LE {$$="<=";};

RETURN_BLOCK: /*DONE!!*/ 
'{' RETRUN_VAL '}' {$$=MakeTree1("BLOCK",$2);}
| '{' BLOCK_BODY RETRUN_VAL '}' {$$=MakeTree2("BLOCK",$2,$3);}; 

BLOCK: /*DONE!!*/
'{' '}'{$$=MakeLeaf("BLOCK NONE");} 
| '{' BLOCK_BODY '}' {$$=MakeTree1("BLOCK",$2);} ;

BLOCK_BODY:/*DONE!!*/
DECLRATIONS STATEMENTS {$$=MakeTree2("",$1,$2);}
| DECLRATIONS {$$=$1;}| STATEMENTS {$$=$1;};

FUNCTIONS: /*DONE!!*/
FUNCTION {$$=$1;}| PROCED {$$=$1;} ;

FUNCTION :/*DONE*/
 FUNC ID '(' PAR_LIST ')' RETURN TYPE RETURN_BLOCK {$$=MakeTree4("FUNC",MakeLeaf($2),$4,MakeTree1("RET",$7),$8);} ;
 
PROCED : /*DONE*/
PROC ID '(' PAR_LIST ')' BLOCK {$$=MakeTree3("PROC",MakeLeaf($2),$4,$6);} ;

MAIN_FUNC : /*DONE*/
PROC MAIN '(' ')' BLOCK {$$=MakeTree1("MAIN",$5);} ;

PAR_LIST: /*DONE!!*/
IDS ':' TYPE {$$=MakeTree1($3->token,MakeLeaf($1));}
|PAR_LIST ';' IDS ':' TYPE {$$=MakeTree2("ARGS",$1,MakeTree1($5->token,MakeLeaf($3)));}
| {$$=MakeLeaf("ARGS NONE");} ;

FUNC_CALL : /*DONE!!*/
ID '(' ')' {$$=MakeTree1("FUNC CALL",MakeLeaf($1));}
|ID '(' EXPS ')' {$$=MakeTree2("FUNC CALL",MakeLeaf($1),$3);};

EXPS: /*DONE!!*/
EXPS ',' EXP {$$=MakeTree2(" ",$1,$3);}
|EXP {$$=MakeTree1(" ",$1);};

ASSIGNMENT : /*DONE!!*/
ID '=' EXP ';' {$$=MakeTree2("=",MakeLeaf($1),$3);}
| STRING_IND '=' EXP ';'  {$$=MakeTree2("=",$1,$3);}
|'^' EXP '=' EXP ';' {$$=MakeTree2("=",MakeTree1("^",$2),$4);}
| PTR_DEC {$$=$1;}  ;

PTR_DEC :/*DONE*/
ID '=' '&' STRING_IND ';' {$$=MakeTree2("=",MakeLeaf($1),MakeTree1("&",$4));};
|ID '=' NULLV ';' {$$=MakeTree2("=",MakeLeaf($1),MakeLeaf("NULL"));}
|ID '=' '&' ID ';' {$$=MakeTree2("=",MakeLeaf($1),MakeTree1("&",MakeLeaf($4)));};

RETRUN_VAL : /*DONE*/
RETURN EXP  ';' {$$=MakeTree1("RET",$2);};

STATEMENTS :/*DONE!!*/
STATEMENTS STATEMENT {$$=MakeTree2("",$1,$2);}
| STATEMENT {$$=$1;}; 

STATEMENT:/*DONE!!*/ 
STATEMENT_WITHOUT_IF {$$=$1;}
| IF_STATEMENT {$$=$1;} ;

DECLRATIONS : /*DONE*/
DECLRATIONS DECLRATION {$$=MakeTree2("",$1,$2);}
| DECLRATION {$$=$1;};

DECLRATION : /*DONE*/
VAR_DECL {$$=$1;}
| STRING_DECL {$$=$1;}
| FUNCTIONS {$$=$1;};

STATEMENT_WITHOUT_IF: /*DONE!!*/
ASSIGNMENT {$$=$1;} 
| FUNC_CALL ';' {$$=$1;} 
| WHILE_STATEMENT {$$=$1;} 
| BLOCK {$$=$1;} 
| RETURN_BLOCK {$$=$1;} ;
            
IF_STATEMENT: /*DONE!!*/
MS {$$=$1;}
|US {$$=$1;};

MS:/*DONE!!*/
IF '(' EXP ')' MS ELSE MS {$$=MakeTree3("IF-ELSE",$3,$5,$7);}
| STATEMENT_WITHOUT_IF {$$=$1;};

US: /*DONE!!*/
IF '(' EXP ')' MS  {$$=MakeTree2("IF",$3,$5);}
| IF '(' EXP ')' US {$$=MakeTree2("IF",$3,$5);}
| IF '(' EXP ')' MS ELSE US {$$=MakeTree3("IF-ELSE",$3,$5,$7);} ;

WHILE_STATEMENT :/*DONE!!*/
 WHILE '(' EXP ')' STATEMENT {$$=MakeTree2("WHILE",$3,$5);} ; 

%%
int yyerror(const char *str)
{
    fprintf(stderr, "%s , line number: %d\n", str,yylineno);
    fprintf(stderr, "parser caused by: '%s'\n",yytext);
}

void PrintSpaces(int i)
{
for( int x=0; x<i ; x++)
	printf(" ");
}

void printtree(node *tree,int t)
{
PrintSpaces(t);
if ( CheckPrint(tree) == 1)
     PrintLeaf(tree);
else
{
    printf("(%s\n", tree->token);

    if(tree->n1)
        printtree(tree->n1,t+6);
    if(tree->n2)
       printtree(tree->n2,t+6);
    if(tree->n3)
       printtree(tree->n3,t+6);
    if(tree->n4)
       printtree(tree->n4,t+6);

    PrintSpaces(t);
    printf(")\n");
}
}

node *MakeTree4(char *token, node *n1, node *n2 ,node *n3,node *n4)
{
node *newnode = (node*)malloc(sizeof(node));
char *newstr = (char*)malloc(sizeof(token) + 1);
strcpy(newstr,token);
newnode->n1 = n1;
newnode->n2 = n2;
newnode->n3 = n3;
newnode->n4 = n4;
newnode->token = newstr;
return newnode;
}

node *MakeLeaf(char *token)
{
return MakeTree4(token,NULL,NULL,NULL,NULL);
}
node *MakeTree1(char *token, node *n1)
{
return MakeTree4(token,n1,NULL,NULL,NULL);
}
node *MakeTree2(char *token, node *n1, node *n2)
{
return MakeTree4(token,n1,n2,NULL,NULL);
}
node *MakeTree3(char *token, node *n1, node *n2 ,node *n3)
{
return MakeTree4(token,n1,n2,n3,NULL);
}

int PrintLeaf(node* tree)
{
 if ( tree->n1 )
     printf("( %s %s ",tree->token,tree->n1->token);
 else{
     printf("%s\n",tree->token);
     return 0;
     }
  if ( tree->n2 )
     printf("%s ",tree->n2->token);

  if ( tree->n3 )
     printf("%s ",tree->n3->token);
     
  if ( tree->n4 )
     printf("%s ",tree->n4->token);
     
  printf(")\n");
  return 0;
       
}
int CheckPrint(node* tree)
{
 int count=IsLeaf(tree->n1)+IsLeaf(tree->n2)+IsLeaf(tree->n3)+IsLeaf(tree->n4);
 
 if(count==4)
    return 1;
    
 return 0;
}

int IsLeaf(node* tree)
{
 if(tree == NULL)
    return 1;
    
 if(tree->n1 == NULL)
    return 1;

 return 0;
}




int main( )
{
 return yyparse( );
 return 0;
}
