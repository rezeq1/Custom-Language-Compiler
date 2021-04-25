#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define false 0
#define true !false

enum
{
 
  Int = 0,
  Char,
  Intp,
  Charp,
  Bool,
  Realp,
  Real,
  String,
  Void,
   Id,
  Func,
  Other,
  Empty,
  Par,
  Block,
  Return,
  Decs,
  If,
  Asign,
  While,
  Do,
  For,
  Abs,
  Ebool,
  Eint,
  Eint2bool,
  Eall2bool,
  Funccall,
  Dref,
  Ref,
  Stringindex,
  Null,
  Not,
  Uminus
};

//===============================================================
//                              AST
//===============================================================
typedef struct Tree
{
  char *string;
  struct Tree *n1, *n2, *n3, *n4;
  int type;
  int lineno;
} Tree;

Tree *make4Tree(char *, int type, Tree *, Tree *, Tree *, Tree *);
Tree *make3Tree(char *, int type, Tree *, Tree *, Tree *);
Tree *make2Tree(char *, int type, Tree *, Tree *);
Tree *make1Tree(char *, int type, Tree *);
Tree *makeLeaf(char *, int type);
void printTree(Tree *, int);
void deleteTree(Tree *);
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
  char *id;
  int isFuncPar;
  int order;
  int size;
  //func
  struct Function * func;

} Entry;

typedef struct Line
{
  int count;
  int Label;
  int hasLabel;
  char line[256];
  struct Line *gto;
  struct Line *next;

} Line;

typedef struct Function
{
  struct Function *next;
  struct Entry *func;
  struct Line *lines;
  int returnType;
  struct Entry *pars;
  int parCount;
  int parSize;
  int varSize;
  int linecount;
  int tempVarCount;

} Function;

typedef struct SymbolTree
{
  struct Entry *table;
  struct SymbolTree *parent;
  struct SymbolTree *child;
  struct SymbolTree *next;
  struct Function *func;
} SymbolTree;

typedef struct Data
{
  struct SymbolTree *golbal;
  struct SymbolTree *current;
  struct Tree *ast;
  int offset;
  int mainCount;
  int lineno;
  // part 3 ;
  Function *funcs;
  Line * nextline;
} Data;

typedef struct Var
{
int type;
char name[128];
} Var;

void test();
void start();
SymbolTree *allocScope(SymbolTree *parent);
void scopeDown();
void scopeUp();
Entry *find(char *id, SymbolTree *current);
int insert(Entry *entry);
void scanTree(Tree *tree);
void scanFunc(Tree *tree);
Entry *scanFuncParam(Tree *tree);
int scanDecs(Tree *tree);
Var scanExpType(Tree *tree);
Var scanFuncCall(Tree *tree);
Entry *scanFuncCallTypes(Tree *tree);
void scanReturn(Tree *tree);
void scanWhile(Tree *tree);
void scanIf(Tree *tree);
Function *getCurrentFunc();
Entry *makeEntry();
Entry *getLast(Entry *);
void printSymbol(SymbolTree *tree, int i);

//part 3 :

Function *makeFunc(Entry *);
Line *makeLine();
Line *getNextLine();
void printFuncsToFile();
void getEntryName(char*,Entry*);
int getTempVar(int type);
int getVarOrder(char*, SymbolTree* );//return count of same name vars in this scope
