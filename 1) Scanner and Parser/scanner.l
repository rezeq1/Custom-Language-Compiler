%{
#include<stdio.h>
#include "y.tab.h"
extern int yylineno;

%}
%x C_COMMENTS
%%


"/%"            { BEGIN(C_COMMENTS); }
<C_COMMENTS>"%/" { BEGIN(INITIAL); }
<C_COMMENTS>\n   {yylineno++;} 
<C_COMMENTS>. { }

  
"Main" {return MAIN;}
"bool" {return BOOL;}

"char" {return CHAR;}
"int" {return INT;}
"real" {return REAL;}
"string" {return STRING;}
"string"[ ]*"["[ ]*[0-9]+[ ]*"]" {return STRINGARR;}
"int*" {return INTP;}
"char*" {return CHARP;}
"real*" {return REALP;}
"if" {return IF;}
"else" {return ELSE;}
"while" {return WHILE;}
"var" {return VAR;}
"func" {return FUNC;}
"proc" {return PROC;}
"return" {return RETURN;}
"null" |"NULL" {return NULLV;}

"&&"  {return AND;}
"/"  {return '/';}
"="  {return '=';}
"=="  {return EQ;}
">"  {return G;}
">="  {return GE;}
"<"  {return L;}
"<="  {return LE;}
"-"  {return '-';}
"!"  {return '!';}
"!="  {return NEQ;}
"||"  {return OR;}
"+" {return '+';}
"*" {return '*';}
"&" {return '&' ;}
"^" {return '^' ;}

";" {return ';' ;}
":" {return ':' ;}
"," {return ',' ;}
"|" {return '|' ;}
"{" {return '{' ;}
"}" {return '}' ;}
"[" {return '[' ;} 
"]" {return ']' ;}
"(" {return '(' ;}
")" {return ')' ;}


"true"|"false" {return BOOLV;}
"'"."'" {yylval.string = yytext; return CHARV;}
[1-9][0-9]*|[0][xX][0-9A-F]+|[0] {yylval.string =strdup(yytext); return INTV;}
(\.[0-9]*)([eE][+-]?[0-9]+)?|[0-9]+(\.[0-9]*)?([eE][+-]?[0-9]+)? {yylval.string =strdup(yytext); return REALV;}
([a-zA-Z][0-9_]*)+ {yylval.string =strdup(yytext); return ID;}
["][^"^\n.]*["] {yylval.string =strdup(yytext); return STRINGV;}

[ \t\f\v\r]  {  }
[\n]  {yylineno++;} 

. {yyerror("Syntax Erorr"); exit(1);}

%%
