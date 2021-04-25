#include "symbol.h"

Data data;
int typeSize[] = {4, 1, 4, 4, 4,4,8,4};
int count=1;

void test()
{
    start();
}

void start()
{
    data.golbal = allocScope(NULL);
    data.current = data.golbal;
    data.offset = 0;
    data.mainCount = 0;
    data.current->func = NULL;
    data.lineno = 0;
    data.funcs = NULL;
    data.nextline = NULL;

    scanTree(data.ast);
    if (data.mainCount != 1)
    {
        printf("error: missing main function!\n");
        exit(1);
    }
    printSymbol(data.golbal, 0);
    printFuncsToFile();
}

SymbolTree *allocScope(SymbolTree *parent)
{
    SymbolTree *t = (SymbolTree *)malloc(sizeof(SymbolTree));
    t->child = NULL;
    t->table = NULL;
    t->next = NULL;
    if (parent != NULL)
        t->func = parent->func;
    else
        t->func = NULL;
    t->parent = parent;
    return t;
}

void scopeDown()
{
    SymbolTree *t = allocScope(data.current);
    SymbolTree *temp;
    if (data.current->child == NULL)
        data.current->child = t;
    else
    {
        temp = data.current->child;
        while (temp->next != NULL)
            temp = temp->next;
        temp->next = t;
    }
    data.current = t;
    data.offset++;
}

void scopeUp()
{
    data.current = data.current->parent;
    data.offset--;
}

Entry *find(char *id, SymbolTree *current)
{
    if (current == NULL)
    {
        printf("error:line: %d id '%s' is not declared!\n", data.lineno, id);
        exit(1);
    }

    Entry *entry = current->table;
    while (entry != NULL)
    {
        if (0 == strcmp(entry->id, id))
            return entry;
        entry = entry->next;
    }
    return find(id, current->parent);
}
int insert(Entry *entry)
{
    SymbolTree *t = data.current;
    Entry *e;

    if (entry->type == Func && 0 == strcmp("Main", entry->id))
    {
        if (data.mainCount >= 1)
        {
            printf("error: function main already exist!\n");
            exit(1);
        }
        data.mainCount++;
    }
    if (entry->type != Func)
    {
        entry->order = getVarOrder(entry->id, data.current);
    }

    if (t->table == NULL)
        t->table = entry;

    else
    {
        e = t->table;
        while (e->next != NULL)
        {

            if (0 == strcmp(e->id, entry->id))
                return -1;
            e = e->next;
        }

        if (0 == strcmp(e->id, entry->id))
            return -1;
        e->next = entry;
    }

    return 1;
}

void scanTree(Tree *tree)
{
    if (tree == NULL)
        return;
    switch (tree->type)
    {

    case Func:
    {
        scanFunc(tree);
    }
    break;

    case Decs:
    {
        scanDecs(tree);
    }
    break;

    case Funccall:
    {
        scanFuncCall(tree);
    }
    break;

    case Return:
    {
        scanReturn(tree);
    }
    break;

    case While:
    {
        scanWhile(tree);
    }
    break;


    case If:
    {
        scanIf(tree);
    }
    break;

    case Block:
    {
        scopeDown();
        scanTree(tree->n1);
        scopeUp();
    }
    break;

    case Asign:
    {
        Var left = scanExpType(tree->n1);
        Var right = scanExpType(tree->n2);
        if(left.type == right.type || (right.type == Null && (left.type == Intp || left.type == Charp || left.type == Realp)))
        {
            Line *line = makeLine();
            sprintf(line->line, "%s = %s", left.name, right.name);
            return;
        }

        printf("error: asingment types doesn't match!\n");
        printTree(tree, 0);
        exit(1);
    }
    break;

    default:
    {
        scanTree(tree->n1);
        scanTree(tree->n2);
        scanTree(tree->n3);
        scanTree(tree->n4);
    };
    }
}
void scanFunc(Tree *tree)
{

    Entry *entry = makeEntry();
    entry->type = Func;
    entry->id = tree->n2->string;
    data.lineno = tree->n2->lineno;
    if (-1 == insert(entry))
    {
        printf("error:line: %d func %s already exist!\n", data.lineno, entry->id);
        exit(1);
    }
    scopeDown();
    Function *fun = makeFunc(entry);
    data.current->func = fun;
    fun->returnType = tree->n1->type;
    Entry *pars = scanFuncParam(tree->n3);
    fun->pars = pars;
    Entry *temp = pars;
    int count = 0;
    if(fun->returnType == String)
    {
       printf("error:line: %d The return type of function cannot be a string \n",data.lineno);
       exit(1);
    }
    while (temp != NULL)
    {
        count++;
        Entry *toinsert = makeEntry();
        toinsert->id = temp->id;
        toinsert->type = temp->type;
        toinsert->isFuncPar = 1;
        if (-1 == insert(toinsert))
        {
            printf("error:line: %d func '%s' parameter '%s' already exist!\n", data.lineno, entry->id, temp->id);
            exit(1);
        }

        temp = temp->next;
    }
    fun->parCount = count;
    if(count !=0 && 0==strcmp("Main",entry->id))
    {
	printf("error: function main dose not accept arguments!\n");
        exit(1);
    }
    scanTree(tree->n4->n1);
    Line *endline = makeLine();
    strcpy(endline->line, "EndFunc");
    scopeUp();
}
Entry *scanFuncParam(Tree *tree)
{
    if (tree->type == Empty)
        return NULL;
    if (tree->type == Other)
    {
        Entry *e = scanFuncParam(tree->n1);
        getLast(e)->next = scanFuncParam(tree->n2);
        return e;
    }
    if (tree->type == Par)
    {
        int type;
        Tree* temp=tree->n1,*exp;
        
        while(temp != NULL ){
           type=temp->type;
           temp=temp->n1;
        }
        
	if(type==String)
	{
	    exp=tree;
	    while(exp->n3 == NULL)
	        exp=exp->n1;
	    
	    if(Int!=scanExpType(exp->n3).type)
	    {
		 printf("error:line: %d string pars have non int index!\n",tree->lineno);
                exit(1);
	    }
	    
       }
        temp=tree->n1;
        Entry* e=makeEntry();
        e->type=type;
        e->id=tree->n2->string;
        data.current->func->parSize+=typeSize[type];
        Entry* temp2=e;
        while(temp->type == Par)
        {
            Entry* next=makeEntry();
            next->type=type;
            next->id=temp->n2->string;
            data.current->func->parSize+=typeSize[type];
            temp2->next=next;
            temp2=temp2->next;
            temp=temp->n1;
	}
	return e;
    }
}
int scanDecs(Tree *tree)
{
    if (tree->type != Decs && tree->type != Other)
        return tree->type;
    if (tree->n2 == NULL)
        return scanDecs(tree->n1);

    int type = scanDecs(tree->n1);
    if (type == String)
    {
        Tree *strdec = tree->n2;
        if (Int != scanExpType(strdec->n2).type)
        {
            printf("error:line: %d string %s have non int index!\n", data.lineno, strdec->n1->string);
            exit(1);
        }
        Entry *e = makeEntry();
        e->id = strdec->n1->string;
        e->type = String;
        e->size = atoi(strdec->n2->string);
        data.lineno = strdec->n1->lineno;
        insert(e);
        data.current->func->varSize += e->size;
        return String;
    }
    else
    {
        Entry *e = makeEntry();
        e->id = tree->n2->string;
        e->type = type;
        data.lineno = tree->n2->lineno;
        data.current->func->varSize += typeSize[type];
        if (-1 == insert(e))
        {
            printf("error:line: %d parameter %s already exist!\n", data.lineno, e->id);
            exit(1);
        }
        return type;
    }
}

Var scanExpType(Tree *tree)
{
    Var result;
    int type = tree->type;
    if (type == Id)
    {
        data.lineno = tree->lineno;
        Entry *e = find(tree->string, data.current);
        result.type = e->type;
        getEntryName(result.name, e);
        return result;
    }
    else if (type == Int || type == Char || type == Bool || type == Null || type == String || type == Real)
    {
        data.lineno = tree->lineno;
        result.type = type;
        strcpy(result.name, tree->string);
        return result;
    }
    else if (type == Abs)
    {
        data.lineno = tree->n1->lineno;
        Entry *e = find(tree->n1->string, data.current);
        if (e->type == Int)
        {
            Var temp;
            getEntryName(temp.name, e);
            result.type = Int;
            int y = getTempVar(Int);
            int x = getTempVar(Bool);
            sprintf(result.name, "t%d", y);
            Line *line = makeLine();
            sprintf(line->line, "%s = %s", result.name, temp.name);

            line = makeLine();
            sprintf(line->line, "t%d = %s < 0 ", x, temp.name);
            Line *linego = makeLine();
            sprintf(linego->line, "if not t%d Goto", x);
            line = makeLine();
            sprintf(line->line, "%s = - %s", result.name, result.name);
            linego->gto = getNextLine();
            return result;
        }
        else if (e->type == String)
        {
            result.type = Int;
            int x = getTempVar(Int);
            sprintf(result.name, "t%d", x);
            Line *line = makeLine();
            sprintf(line->line, "%s = %d", result.name, e->size);
            return result;
        }
        else
        {
            printf("error:line: %d operator ABS is used on wrong type of id '%s'\n", data.lineno, tree->n1->string);
            exit(1);
        }
    }
    else if (type == Ebool)
    {

        Var left = scanExpType(tree->n1);
        Var right;
        int x = getTempVar(Bool);
        sprintf(result.name, "t%d", x);

        if (0 == strcmp(tree->string, "AND"))
        {
            Line *line = makeLine();
            sprintf(line->line, "%s = %s", result.name, left.name);

            Line *linego = makeLine();
            sprintf(linego->line, "if not %s Goto", result.name); //goto the end

            right = scanExpType(tree->n2);

            line = makeLine();
            sprintf(line->line, "%s = %s", result.name, right.name);

            linego->gto = getNextLine();
        }

        else if (0 == strcmp(tree->string, "OR"))
        {
            Line *line = makeLine();
            sprintf(line->line, "%s = %s", result.name, left.name);

            Line *linego = makeLine();
            sprintf(linego->line, "if not %s Goto", result.name); //goto the end
            Line *linego2 = makeLine();
            sprintf(linego2->line, "Goto"); //goto the end
            linego->gto = getNextLine();
            right = scanExpType(tree->n2);

            line = makeLine();
            sprintf(line->line, "%s = %s", result.name, right.name);
            
            linego2->gto = getNextLine();
        }

        if (left.type == Bool && right.type == Bool)
        {
            result.type = Bool;
            return result;
        }
        printf("error:line: %d operator '%s' have wrong types\n", data.lineno, tree->string);
        exit(1);
    }
    else if (type == Not)
    {
        Var exp = scanExpType(tree->n1);
        if (exp.type == Bool)
        {
            int x = getTempVar(Bool);
            Line *linego = makeLine();
            sprintf(linego->line, "if not %s Goto", exp.name);
            Line *line = makeLine();
            sprintf(line->line, "t%d = 0", x);
            Line *linego2 = makeLine();
            sprintf(linego2->line, "Goto");
            line = makeLine();
            sprintf(line->line, "t%d = 1", x);
            linego->gto = line;
            linego2->gto = getNextLine();

            sprintf(result.name, "t%d", x);
            result.type = Bool;
            return result;
        }
        printf("error:line: %d operator '%s' have wrong types\n", data.lineno, tree->string);
        exit(1);
    }
    else if (type == Uminus)
    {
        Var exp = scanExpType(tree->n1);
        if (exp.type == Int)
        {
            int x = getTempVar(exp.type);
            Line *line = makeLine();
            sprintf(line->line, "t%d = - %s", x, exp.name);
            result.type = exp.type;
            sprintf(result.name, "t%d", x);
            return result;
        }
        printf("error:line: %d operator '%s' have wrong types\n", data.lineno, tree->string);
        exit(1);
    }
    if (type == Eall2bool)
    {
        Var left = scanExpType(tree->n1);
        Var right = scanExpType(tree->n2);

        int x = getTempVar(Bool);
        sprintf(result.name, "t%d", x);
        Line *line = makeLine();

        if (0 == strcmp(tree->string, "=="))
            sprintf(line->line, "%s = %s == %s", result.name, left.name, right.name);
        else
        {
            sprintf(line->line, "%s = %s == %s", result.name, left.name, right.name);
            int y = getTempVar(Bool);
            Line *linego = makeLine();
            sprintf(linego->line, "if not t%d Goto", x);
            line = makeLine();
            sprintf(line->line, "t%d = 0", y);
            Line *linego2 = makeLine();
            sprintf(linego2->line, "Goto");
            line = makeLine();
            sprintf(line->line, "t%d = 1", y);
            linego->gto = line;
            linego2->gto = getNextLine();

            sprintf(result.name, "t%d", y);
        }
        if (left.type == right.type)
        {
            result.type = Bool;
            return result;
        }
        else if (left.type == Null || right.type == Null)
        {
            if (left.type == Realp || right.type == Realp || left.type == Intp || left.type == Charp || right.type == Intp || right.type == Charp || (left.type == Null && right.type == Null))
            {
                result.type = Bool;
                return result;
            }
        }

        printf("error:line: %d operator '%s' have wrong types\n", data.lineno, tree->string);
        exit(1);
    }
    if (type == Eint2bool)
    {
        Var left = scanExpType(tree->n1);
        Var right = scanExpType(tree->n2);
        int x = getTempVar(Bool);
        sprintf(result.name, "t%d", x);
        Line *line = makeLine();

        if (0 == strcmp(tree->string, "<"))
            sprintf(line->line, "%s = %s < %s", result.name, left.name, right.name);
        else if (0 == strcmp(tree->string, ">"))
            sprintf(line->line, "%s = %s < %s", result.name, right.name, left.name);
        else
        {
            int y = getTempVar(Bool);
            int z = getTempVar(Bool);
            sprintf(result.name, "t%d", z);
            if (0 == strcmp(tree->string, "<="))
            {
                sprintf(line->line, "t%d = %s < %s", x, left.name, right.name);
                line = makeLine();
                sprintf(line->line, "t%d = %s == %s", y, right.name, left.name);
                line = makeLine();
                sprintf(line->line, "%s = t%d || t%d", result.name, x, y);
            }
            else if (0 == strcmp(tree->string, ">="))
            {
                sprintf(line->line, "t%d = %s < %s", x, right.name, left.name);
                line = makeLine();
                sprintf(line->line, "t%d = %s == %s", y, right.name, left.name);
                line = makeLine();
                sprintf(line->line, "%s = t%d || t%d", result.name, x, y);
            }
        }

        if (left.type == Int || left.type == Real)
            if (right.type == Int || right.type == Real)
            {
                result.type = Bool;
                return result;
            }
        printf("error:line: %d operator '%s' have wrong types\n", data.lineno, tree->string);
        exit(1);
    }
    if (type == Eint)
    {
        Var left = scanExpType(tree->n1);
        Var right = scanExpType(tree->n2);

        int x = getTempVar(Int);
        sprintf(result.name, "t%d", x);

        Line *line = makeLine();
        sprintf(line->line, "%s = %s %s %s", result.name, left.name, tree->string, right.name);

        if (left.type == Int)
            if (right.type == Int)
            {
                result.type = Int;
                return result;
            }
            
       if(left.type == Int || left.type ==Real)
            if(right.type == Int || right.type == Real)
              {
                result.type = Real;
                return result;
              }
            
        printf("error:line: %d operator '%s' have wrong types\n", data.lineno, tree->string);
        exit(1);
    }
    if (type == Dref)
    {
        printTree(tree, 0);
        Var exp = scanExpType(tree->n1);
        if (exp.type == Intp)
        {
            result.type = Int;
            return result;
        }
        if (exp.type == Charp)
        {
            result.type = Char;
            return result;
        }
        if (exp.type == Realp)
        {
            result.type = Real;
            return result;
        }
        int x = getTempVar(Bool);
        sprintf(result.name, "t%d", x);

        Line *line = makeLine();
        sprintf(line->line, "%s = *%s", result.name, exp.name);

        printf("error:line: %d derference wrong id '%s' type\n", data.lineno, tree->n1->string);
        exit(1);
    }
    if (type == Ref)
    {
        Var exp = scanExpType(tree->n1);
        if (exp.type == Int)
        {
            result.type = Intp;
            return result;
        }
        if (exp.type == Char)
        {
            result.type = Charp;
            return result;
        }
        if (exp.type == Real)
        {
            result.type = Realp;
            return result;
        }
        int x = getTempVar(Bool);
        sprintf(result.name, "t%d", x);

        Line *line = makeLine();
        sprintf(line->line, "%s = &%s", result.name, exp.name);

        printf("error:line: %d rference wrong type: '%s'\n", data.lineno, tree->n1->string);
        exit(1);
    }
    if (type == Stringindex)
    {

        Var stringexp = scanExpType(tree->n1);
        Var indexexp = scanExpType(tree->n2);

        int x = getTempVar(Charp);
        int y = getTempVar(Char);
        Line *line = makeLine();
        sprintf(line->line, "t%d = %s + %s", x, stringexp.name, indexexp.name);
        line = makeLine();
        sprintf(line->line, "t%d = *t%d", y, x);
        sprintf(result.name, "t%d", x);
        if (stringexp.type != String)
        {
            printf("error:line: %d wrong opertator [] '%s' not string\n", data.lineno, tree->n1->string);
            exit(1);
        }
        if (indexexp.type != Int)
        {
            printf("error:line: %d wrong index in opertator %s[] \n", data.lineno, tree->n1->string);
            exit(1);
        }

        result.type = Char;
        return result;
    }
    if (type == Funccall)
        return scanFuncCall(tree);

    result.type = Other;
    return result;
}
Var scanFuncCall(Tree *tree)
{
    Var result;
    data.lineno = tree->n1->lineno;
    Function *func = find(tree->n1->string, data.current)->func;
    int returntype = func->returnType;
    Entry *pars = func->pars;
    Entry *callTypes = scanFuncCallTypes(tree->n2);
    while (pars != NULL && callTypes != NULL)
    {
        if (pars->type != callTypes->type)
        {
            printf("error:line: %d wrong function parameter type for '%s' call\n", data.lineno, tree->n1->string);
            exit(1);
        }
        pars = pars->next;
        callTypes = callTypes->next;
    }
    if (pars != NULL || callTypes != NULL)
    {
        printf("error:line: %d wrong number of parameters for function '%s' call\n", data.lineno, tree->n1->string);
        exit(1);
    }
    if (returntype == Void)
    {
        Line *line = makeLine();
        getEntryName(result.name, func->func);
        sprintf(line->line, "LCall %s", result.name);
    }
    else
    {
        int x = getTempVar(returntype);
        Line *line = makeLine();
        getEntryName(result.name, func->func);
        sprintf(line->line, "t%d = LCall %s", x, result.name);
        sprintf(result.name, "t%d", x);
    }
    if (func->parSize != 0)
    {
        Line *line = makeLine();
        sprintf(line->line, "PopParams %d", func->parSize);
    }

    result.type = returntype;
    return result;
}
Entry *scanFuncCallTypes(Tree *tree)
{
    if (tree == NULL)
        return NULL;
    if (tree->n2 == NULL)
    {
        Var exp = scanExpType(tree->n1);
        Line *line = makeLine();
        sprintf(line->line, "PushParam %s", exp.name);
        Entry *e = makeEntry();
        e->type = exp.type;
        return e;
    }
    Entry *e = makeEntry();

    Var exp = scanExpType(tree->n2);
    Line *line = makeLine();
    sprintf(line->line, "PushParam %s", exp.name);
    e->type = exp.type;
    Entry *list = scanFuncCallTypes(tree->n1);
    getLast(list)->next = e;
    return list;
}
void scanReturn(Tree *tree)
{
    Function *fun = getCurrentFunc();
    Var ret = scanExpType(tree->n1);
    Line *line = makeLine();
    sprintf(line->line, "Return %s", ret.name);
    if (tree->n1 == NULL && fun->returnType != Void)
    {
        printf("error:line: %d wrong return type NULL for func '%s' l\n", data.lineno, fun->func->id);
        exit(1);
    }
    if (tree->n1 != NULL && ret.type != fun->returnType)
    {
        printf("error:line: %d mismatching return type for func '%s' \n", data.lineno, fun->func->id);
        exit(1);
    }
}

void scanWhile(Tree *tree)
{
    Line *target = getNextLine();
    Var exp = scanExpType(tree->n1);

    Line *linego = makeLine();
    sprintf(linego->line, "if not %s Goto", exp.name);

    if (Bool != exp.type)
    {
        printf("error:line: %d while expression is not boolean \n", data.lineno);
        printTree(tree->n1, 0);
        exit(1);
    }
    scopeDown();
    if (tree->n2->type == Block)
        scanTree(tree->n2->n1);
    else
        scanTree(tree->n2);
    scopeUp();

    Line *line = makeLine();
    sprintf(line->line, "Goto");
    line->gto = target;
    linego->gto = getNextLine();
}

void scanIf(Tree *tree)
{
    Var exp = scanExpType(tree->n1);
    Line *linego = makeLine();
    sprintf(linego->line, "if not %s Goto", exp.name);
    if (Bool != exp.type)
    {
        printf("error:line: %d if expression is not boolean \n", data.lineno);
        printTree(tree->n1, 0);
        exit(1);
    }
    scopeDown();
    if (tree->n2->type == Block)
        scanTree(tree->n2->n1);
    else
        scanTree(tree->n2);
    scopeUp();
    scopeDown();
    if (tree->n3 != NULL)
    {
        Line *linego2 = makeLine();
        sprintf(linego2->line, "Goto");
        linego->gto = getNextLine();
        if (tree->n3->type == Block)
            scanTree(tree->n3->n1);
        else
            scanTree(tree->n3);
        linego2->gto = getNextLine();
    }
    else
        linego->gto = getNextLine();
    scopeUp();
}
Function *getCurrentFunc()
{
    SymbolTree *sym = data.current;
    while (sym != NULL)
    {
        if (sym->func != NULL)
            return sym->func;
        sym = sym->parent;
    }
    return NULL;
}
Entry *makeEntry()
{
    Entry *e = (Entry *)malloc(sizeof(Entry));
    e->next = NULL;
    e->isFuncPar = 0;
    e->order = 0;
    return e;
}
Entry *getLast(Entry *e)
{
    while (e->next != NULL)
        e = e->next;
    return e;
}

void printSymbol(SymbolTree *tree, int i)
{
    if (tree == NULL)
        return;
    if (tree->table != NULL)
    {
        Entry *e = tree->table;
        while (e != NULL)
        {
            for (int j = 0; j < i; j++)
                printf("   |");
            printf("%s\n", e->id);
            e = e->next;
        }
    }
    printSymbol(tree->child, i + 1);
    if (tree->next != NULL)
    {
        for (int j = 0; j < i - 1; j++)
            printf("   |");
        printf("\n");
    }
    printSymbol(tree->next, i);
}

Function *makeFunc(Entry *e)
{
    Function *f = (Function *)malloc(sizeof(Function));
    e->func = f;
    f->next = NULL;
    f->func = e;
    f->linecount = 0;
    f->lines = NULL;
    f->varSize = 0;
    f->parSize = 0;
    f->parCount = 0;
    f->tempVarCount = 0;
    if (data.funcs == NULL)
        data.funcs = f;
    else
    {
        //find function order
        int order = 0;
        Function *temp = data.funcs;
        Function *last = NULL;
        while (temp != NULL)
        {
            if (0 == strcmp(e->id, temp->func->id))
                order++;
            last = temp;
            temp = temp->next;
        }
        printf("making %s\n", e->id);
        last->next = f;
        e->order = order;
    }
    return f;
}
Line *makeLine()
{
    Line *l;
    if (data.nextline == NULL)
    {
        l = (Line *)malloc(sizeof(Line));
        l->hasLabel = false;
    }
    else
        l = data.nextline;
    data.nextline = NULL;
    l->gto = NULL;
    l->next = NULL;

    Function *func = data.current->func;
    func->linecount++;
    l->count = func->linecount;
    if (func->lines == NULL)
        func->lines = l;
    else
    {
        Line *last, *temp = func->lines;

        while (temp != NULL)
        {
            last = temp;
            temp = temp->next;
        }
        last->next = l;
    }
    return l;
}

Line *getNextLine()
{
    if (data.nextline == NULL)
        data.nextline = (Line *)malloc(sizeof(Line));
    data.nextline->hasLabel = true;
    data.nextline->Label=count;
    count++;
    return data.nextline;
}

void printFuncsToFile()
{
    int offset = 0;
    FILE *out = fopen("output.txt", "w+");
    Function *func = data.funcs;
    char name[256], temp[128];
    while (func != NULL)
    {
        getEntryName(name, func->func);
        fprintf(out, "%s:\r\n", name);
        fprintf(out, "         BeginFunc \r\n");
        offset++;
        Line *line = func->lines;
        while (line != NULL)
        {
            if (line->hasLabel)
            {
                sprintf(temp, "L%d:", line->Label);
                sprintf(name, "%-9s", temp);
            }
            else
                sprintf(name, "         ");

            if (line->gto != NULL)
            {
                Line *target = line->gto;
                target->hasLabel = true;
                fprintf(out, "%s%s L%d\r\n", name, line->line,target->Label);
            }
            else
                fprintf(out, "%s%s\r\n", name, line->line);
            line = line->next;
        }
        offset += func->linecount;
        func = func->next;
    }
}
void getEntryName(char *name, Entry *e)
{
    if (e->order == 0)
        sprintf(name, "%s", e->id);
    else
        sprintf(name, "%s%d", e->id, e->order);
}

int getTempVar(int type)
{
    data.current->func->varSize += typeSize[type];
    return data.current->func->tempVarCount++;
}

//return count of same name vars in this scope
int getVarOrder(char *id, SymbolTree *current)
{
    if (current == NULL)
        return 0;

    Entry *entry = current->table;
    while (entry != NULL)
    {
        if (0 == strcmp(entry->id, id))
            return entry->order + 1;
        entry = entry->next;
    }
    return getVarOrder(id, current->parent);
}
