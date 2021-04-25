%{
#include <stdio.h>
#include <stdlib.h>
#include "y.tab.h" 

extern int yylineno;

void yyerror(char *);

char* temp;

%}
%x C_COMMENT
%%

"//".*[\n] { yylineno++;}


"/%"            { BEGIN(C_COMMENT); }
<C_COMMENT>"%/" { BEGIN(INITIAL); }
<C_COMMENT>\n   {yylineno++;} 
<C_COMMENT>. { }

[ \t\f\v\r]  { }
[\n]  {yylineno++;}    

"bool" {return BOOL;}
"char" {return CHAR;}
"proc" {return VOID;}
"func" {return FUN;}
"var"  {return VAR;}
"Main" {yylval.string = strdup(yytext) ; return MAINID;}
"int" {return INT;}
"string" {return STRING;}
"int*" {return INTP;}
"real*" {return REALP;}
"real" {return REAL;}
"char*" {return CHARP;}
"if" {return IF;}
"else" {return ELSE;}
"while" {return WHILE;}
"return" {return RETURN;}
"null" {return NUL;}

"&&" {return AND_OP;}
"/" {return '/';}
"=" {return '=';}
":" {return ':';}
"==" {return EQ_OP;}
">" {return '>';}
">=" {return GE_OP;}
"<" {return '<';}
"<=" {return LE_OP;}
"-" {return '-';}
"!" {return '!';}
"!=" {return NE_OP;}
"||" {return OR_OP;}
"+" { return '+';} 
"*" {return '*';}
"&" {return '&';}
"^" {return '^';}

"true" { return TRUE; }
"false" { return FALSE; }

";" {return ';';}
"," {return ',';}
"|" {return '|';}
"{" { return('{');}
"}" { return '}';}
"(" { return '(';}
")" {return ')';}
"[" {return '[';}
"]" {return ']';}

"'"."'" { temp = strdup(yytext) ; temp[yyleng-1]='\0';strcpy(temp,temp+1);yylval.string=temp;return CONST_CHAR;}
0|([1-9][0-9]*) { yylval.string=strdup(yytext) ;return CONST_INT;}
0[xX][a-fA-F0-9]+ { yylval.string=strdup(yytext) ; return CONST_INT;}
0[1-7][0-7]* {yylval.string=strdup(yytext) ; return CONST_INT;}
[01]+b {  yylval.string=strdup(yytext); return CONST_INT;}
(\.[0-9]*)([eE][+-]?[0-9]+)?|[0-9]+(\.[0-9]*)?([eE][+-]?[0-9]+)? { yylval.string=strdup(yytext) ;return CONST_REAL;}
\"([^\"])*\" { temp = strdup(yytext) ; temp[yyleng-1]='\0';strcpy(temp,temp+1);yylval.string=temp ; return CONST_STRING;}

([a-zA-Z][0-9_]*)+ { yylval.string = strdup(yytext) ; return ID;}

.  { yyerror("incorrect character"); exit(1); }
%%
