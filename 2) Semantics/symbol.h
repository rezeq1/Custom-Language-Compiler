#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum { Id, Int , Char , String , Intp, Charp, Bool, Void , Func, Other,
        Empty, Par,Block,Return, Decs,If,Asign,
        While,Abs,Ebool,Eint,Eint2bool,Eall2bool,Funccall,Dref,
        Ref,Stringindex,Null,Real,Realp};

//===============================================================
//                              AST
//===============================================================
 typedef struct Tree 
 {
   char* string;
   struct Tree *n1, *n2, *n3, *n4;
   int type;
   int lineno;
 } Tree;

Tree* make4Tree(char*,int type, Tree*, Tree*, Tree*, Tree*);
Tree* make3Tree(char*,int type, Tree*, Tree*, Tree*);
Tree* make2Tree(char*,int type, Tree*, Tree*);
Tree* make1Tree(char*,int type, Tree*);
Tree* makeLeaf(char*,int type);
void printTree(Tree*,int);
void deleteTree(Tree*);
void PutExp(Tree* ,Tree*);
void HaveReturn(Tree* decs ,int* have);
void CheckHaveReturn(Tree* decs,char* FuncName);

//==============================================================
//                      Symbol Table
//==============================================================



 typedef struct Entry 
 {
    struct Entry *next;
   int type;
   char * id;
   int returnType;
   int isFuncPar;
   struct Entry * pars;
   int parCount;
   

 } Entry;

 typedef struct SymbolTree
 {
    struct Entry *table;
    struct SymbolTree *parent;
    struct SymbolTree *child;
    struct SymbolTree *next;
    struct Entry *func;
 } SymbolTree;

 typedef struct Data
{
    struct SymbolTree * golbal;
    struct SymbolTree * current;
    struct Tree * ast;
    int offset;
    int mainCount;
    int lineno;
} Data;

void test();
void start();
SymbolTree * allocScope(SymbolTree * parent);
void scopeDown();
void scopeUp();
 Entry* find(char * id, SymbolTree* current);
  int insert(Entry *entry);
void scanTree(Tree* tree);
void scanFunc(Tree* tree);
Entry* scanFuncParam(Tree* tree);
int scanDecs(Tree* tree);
int scanExpType(Tree* tree);
int scanFuncCall(Tree* tree);
Entry* scanFuncCallTypes(Tree *tree);
void scanReturn(Tree* tree);
void scanWhile(Tree* tree);
void scanIf(Tree* tree);
Entry* getCurrentFunc();
Entry* makeEntry();
Entry * getLast(Entry*);
void printSymbol(SymbolTree* tree, int i);


