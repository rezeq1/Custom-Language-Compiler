#include "symbol.h"

Data data;
 
 void test()
 {
     start();
 }

 void start()
 {
     data.golbal=allocScope(NULL);
     data.current=data.golbal;
     data.offset=0;
     data.mainCount=0;
     data.current->func=NULL;
     data.lineno=0;
     scanTree(data.ast);
     if(data.mainCount!=1)
     {
         printf("error: missing main function!\n");
            exit(1);
     }
     printSymbol(data.golbal,0);

 }

 SymbolTree * allocScope(SymbolTree * parent)
 {
     SymbolTree * t=(SymbolTree*) malloc (sizeof(SymbolTree));
     t->child=NULL;
     t->table=NULL;
     t->next=NULL;
     t->func=NULL;
     t->parent=parent;
     return t;
 }

 void scopeDown()
 {
     SymbolTree * t=allocScope(data.current);
      SymbolTree * temp;
     if (data.current->child==NULL)
        data.current->child=t;
    else
    {
        temp=data.current->child;
        while(temp->next!=NULL)
            temp=temp->next;
        temp->next=t;
    }
    data.current=t;
    data.offset++;
 }

void scopeUp()
{
    data.current=data.current->parent;
    data.offset--;
}

 Entry* find(char * id, SymbolTree* current)
 {
     if (current==NULL)
        {
            printf("error:line: %d id '%s' is not declared!\n",data.lineno,id);
            exit(1);
        }

    Entry* entry=current->table;
    while (entry!=NULL)
    {
        if (0==strcmp(entry->id,id))
            return entry;
        entry=entry->next;
    }
    return find(id,current->parent);
 }
 int insert(Entry *entry)
 {
    SymbolTree * t=data.current;
    Entry *e;

    if(entry->type==Func && 0==strcmp("Main",entry->id))
    {
        if(data.mainCount>=1)
        {
            printf("error: function main already exist!\n");
            exit(1);
        }
        data.mainCount++;
    }

    if (t->table==NULL)
    {
        t->table=entry;
        return 1;
    }
    e=t->table;
    while(e->next!=NULL)
       {

           if(0==strcmp(e->id,entry->id))
            return -1;
            e=e->next;

       }

       
       if(0==strcmp(e->id,entry->id))
            return -1;
    e->next=entry;
    return 1;
 }

 void scanTree(Tree* tree)
 {
     if(tree==NULL)
        return;
     switch(tree->type) 
     {

        case Func  :
        {
         scanFunc(tree);
        }
        break; 
            
        case Decs  :
        {
         scanDecs(tree);
        }
        break;  

        case Funccall  :
        {
         scanFuncCall(tree);
        }
        break;  

         case Return  :
        {
         scanReturn(tree);
        }
        break; 

        case While  :
        {
         scanWhile(tree);
        }
        break; 

        case If  :
        {
         scanIf(tree);
        }
        break; 

        case Block  :
        {
            scopeDown();
            scanTree(tree->n1);
            scopeUp();
        }
        break; 

         case Asign  :
        {
            int left=scanExpType(tree->n1);
            int right=scanExpType(tree->n2);
        if (left==right)
            return;
        if(right==Null &&(left==Intp||left==Charp||left==Realp))
            return;
         
        printf("error: asingment types doesn't match!\n");
        printTree(tree,0);
        exit(1);
            
        }
        break; 
        
        default : 
        {
            scanTree(tree->n1);
            scanTree(tree->n2);
            scanTree(tree->n3);
            scanTree(tree->n4);
        };
    }
 }
void scanFunc(Tree* tree)
{
    
   Entry* entry = makeEntry();
   entry->returnType=tree->n1->type;
   entry->type=Func;
   entry->id=tree->n2->string;
   data.lineno=tree->n2->lineno;
   if(-1==insert(entry))
   {
       printf("error:line: %d func %s already exist!\n",data.lineno,entry->id);
       exit(1);
   }
   if(entry->returnType == String)
   {
       printf("error:line: %d The return type of function cannot be a string \n",data.lineno);
       exit(1);
   }
   scopeDown();
   data.current->func=entry;
    Entry* pars=scanFuncParam(tree->n3);
    entry->pars=pars;
    Entry * temp=pars;
    int count=0;

    while (temp!=NULL)
    {
        count++;
        Entry *toinsert=makeEntry();
        toinsert->id=temp->id;
        toinsert->type=temp->type;
        toinsert->isFuncPar=1;
        if(-1==insert(toinsert))
        {
            printf("error:line: %d func '%s' parameter '%s' already exist!\n",data.lineno,entry->id,temp->id);
       exit(1);
        }

        temp=temp->next;
    }
    entry->parCount=count;
    if(count !=0 && 0==strcmp("Main",entry->id))
        {
            printf("error: function main dose not accept arguments!\n");
            exit(1);
        }
        scanTree(tree->n4->n1);
    scopeUp();
}
Entry* scanFuncParam(Tree* tree)
{
    if (tree->type==Empty)
        return NULL;
    if (tree->type==Other)
    {
        Entry* e=scanFuncParam(tree->n1);
        getLast(e)->next=scanFuncParam(tree->n2);
        return e;
    }
    
    if(tree->type==Par)
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
	    
	    if(Int!=scanExpType(exp->n3))
	    {
		 printf("error:line: %d string pars have non int index!\n",tree->lineno);
                exit(1);
	    }
       }
        temp=tree->n1;
        Entry* e=makeEntry();
        e->type=type;
        e->id=tree->n2->string;
        Entry* temp2=e;
        while(temp->type == Par)
        {
            Entry* next=makeEntry();
            next->type=type;
            next->id=temp->n2->string;
            temp2->next=next;
            temp2=temp2->next;
            temp=temp->n1;
	}
	return e;
	
    }
    
}


int scanDecs(Tree* tree)
{
    if(tree->type!=Decs && tree->type!=Other)
        return tree->type;
    if (tree->n2==NULL)
        return scanDecs(tree->n1);

    int type=scanDecs(tree->n1);
    if(type==String)
    {
        Tree* strdec=tree->n2;
        if(Int!=scanExpType(strdec->n2))
        {
              printf("error:line: %d string %s have non int index!\n",data.lineno,strdec->n1->string);
            exit(1);
        }
        Entry* e=makeEntry();
        e->id=strdec->n1->string;
        e->type=String;
        data.lineno=strdec->n1->lineno;
        insert(e);
        return String;

    }
    else
    {
        Entry* e=makeEntry();
        e->id=tree->n2->string;
        e->type=type;
        data.lineno=tree->n2->lineno;
        if (-1==insert(e))
        {
            printf("error:line: %d parameter %s already exist!\n",data.lineno,e->id);
            exit(1);
              
        }
        return type;
    }


}

int scanExpType(Tree* tree)
{
    int type=tree->type;
    if (type==Id)
            {
                data.lineno=tree->lineno;
                return find(tree->string,data.current)->type;
            }
    if(type==Real || type==Int || type==Char || type==Bool || type==Null || type==String)
        {
            data.lineno=tree->lineno;
            return type;
        }
    if (type==Abs)
    {
        data.lineno=tree->n1->lineno;
        int abstype=find(tree->n1->string,data.current)->type;
        if(abstype==Int || abstype==String)
            return Int;
        else
        {
            printf("error:line: %d operator ABS is used on wrong type of id '%s'\n",data.lineno,tree->n1->string);
            exit(1);
        }
    }
    if(type==Ebool)
    {
        if(scanExpType(tree->n1)==Bool)
            if(tree->n2==NULL || scanExpType(tree->n2)==Bool)
                return Bool;
        printf("error:line: %d operator '%s' have wrong types\n",data.lineno,tree->string);
        exit(1);
    }
    if(type==Eall2bool)
    {
        int left=scanExpType(tree->n1);
        int right=scanExpType(tree->n2);
        if(left==right)
           return Bool;
        else if (left==Null|| right==Null)
        {
            if(left==Realp||right==Realp||left==Intp||left==Charp||right==Intp||right==Charp||(left==Null&& right==Null))
            return Bool;
        }

        printf("error:line: %d operator '%s' have wrong types\n",data.lineno,tree->string);
        exit(1);
        
    }
    if(type==Eint2bool)
    {
         if(scanExpType(tree->n1)==Int ||scanExpType(tree->n1)==Real)
            if(scanExpType(tree->n2)==Int || scanExpType(tree->n2)==Real)
                return Bool;
         printf("error:line: %d operator '%s' have wrong types\n",data.lineno,tree->string);
         exit(1);
    }
    if(type==Eint)
    {
        if(scanExpType(tree->n1)==Int)
            if(scanExpType(tree->n2)==Int)
                return Int;
                
        if(scanExpType(tree->n1)==Int || scanExpType(tree->n1)==Real)
            if(scanExpType(tree->n2)==Int || scanExpType(tree->n2)==Real)
                return Real;
                
         printf("error:line: %d operator '%s' have wrong types\n",data.lineno,tree->string);
         exit(1);
    }
    if(type==Dref)
    {
        
        printTree(tree,0);
        int derftype = scanExpType(tree->n1);
        if(derftype==Intp)
            return Int;
        if(derftype==Charp)
            return Char;
        if(derftype==Realp)
            return Real;
        printf("error:line: %d derference wrong id '%s' type\n",data.lineno,tree->n1->string);
         exit(1);

    }
    if(type==Ref)
    {
       int reftype=scanExpType(tree->n1);
        if(reftype==Int)
            return Intp;
        if(reftype==Char)
            return Charp;
        if(reftype==Real)
            return Realp;
         printf("error:line: %d rference wrong type: '%s'\n",data.lineno,tree->n1->string);
         exit(1);      
    }
    if(type==Stringindex)
    {
        if(scanExpType(tree->n1)!=String)
        {
             printf("error:line: %d wrong opertator [] '%s' not string\n",data.lineno,tree->n1->string);
         exit(1);   
        }
        if(scanExpType(tree->n2)!=Int)
            {
             printf("error:line: %d wrong index in opertator %s[] \n",data.lineno,tree->n1->string);
         exit(1);   
        }
        return Char;
    }
    if(type==Funccall)
        return scanFuncCall(tree);
    return Other;
}
int scanFuncCall(Tree* tree)
{
    data.lineno=tree->n1->lineno;
    Entry *fun=find(tree->n1->string,data.current);
    int returntype=fun->returnType;
    fun=fun->pars;
    Entry *callTypes=scanFuncCallTypes(tree->n2);
    while(fun!=NULL && callTypes!=NULL)
    {
        if (fun->type!=callTypes->type)
        {
             printf("error:line: %d wrong function parameter type for '%s' call\n",data.lineno,tree->n1->string);
         exit(1); 
        }
        fun=fun->next;
        callTypes=callTypes->next;
    }
    if(fun!=NULL || callTypes !=NULL)
    {
         printf("error:line: %d wrong number of parameters for function '%s' call\n",data.lineno,tree->n1->string);
         exit(1);
    }
    return returntype;
}
Entry* scanFuncCallTypes(Tree *tree)
{
    if (tree==NULL)
    return NULL;
    if(tree->n2==NULL)
    {
        Entry *e=makeEntry();
        e->type = scanExpType(tree->n1);
        return e;
    }
    Entry *e=makeEntry();
     e->type = scanExpType(tree->n2);
    Entry *list=scanFuncCallTypes(tree->n1);
    getLast(list)->next=e;
    return list;
}
void scanReturn(Tree* tree)
{
    Entry* fun=getCurrentFunc();
    if(tree->n1==NULL && fun->returnType!=Void)
    {
         printf("error:line: %d wrong return type NULL for func '%s' l\n",data.lineno,fun->id);
         exit(1);
    }
    if(tree->n1!=NULL &&scanExpType(tree->n1)!=fun->returnType)
    {
         printf("error:line: %d mismatching return type for func '%s' \n",data.lineno,fun->id);
         exit(1);
    }

}

void scanWhile(Tree* tree)
{
    if(Bool!=scanExpType(tree->n1))
    {
        printf("error:line: %d while expression is not boolean \n",data.lineno);
        printTree(tree->n1,0);
         exit(1);
    }
    scopeDown();
    if (tree->n2->type==Block)
        scanTree(tree->n2->n1);
    else
        scanTree(tree->n2);
    scopeUp();
}

void scanIf(Tree* tree)
{
     if(Bool!=scanExpType(tree->n1))
    {
        printf("error:line: %d if expression is not boolean \n",data.lineno);
        printTree(tree->n1,0);
         exit(1);
    }
    scopeDown();
    if (tree->n2->type==Block)
        scanTree(tree->n2->n1);
    else
        scanTree(tree->n2);

    if(tree->n3!=NULL)
    {
         if (tree->n3->type==Block)
        scanTree(tree->n3->n1);
    else
        scanTree(tree->n3);
    }
    scopeUp();
}
Entry* getCurrentFunc()
{
    SymbolTree* sym=data.current;
    while(sym!=NULL)
    {
        if(sym->func!=NULL)
            return sym->func;
        sym=sym->parent;
    }
    return NULL;
}
Entry* makeEntry()
{
    Entry *e=(Entry*) malloc (sizeof(Entry));
    e->next=NULL;
    e->isFuncPar=0;
    e->pars=NULL;
    return e;
}
Entry * getLast(Entry* e)
{
    while(e->next!=NULL)
        e=e->next;
    return e;
}

void printSymbol(SymbolTree* tree, int i)
{
if (tree==NULL)
return;
if (tree->table!=NULL)
{
    Entry *e=tree->table;
    while(e!=NULL)
    {
         for(int j=0;j<i;j++)
            printf("   |");
        printf("%s\n",e->id);
        e=e->next;
    }
}
printSymbol(tree->child,i+1);
if(tree->next!=NULL)
{
    for(int j=0;j<i-1;j++)
        printf("   |");
    printf("\n");
}
printSymbol(tree->next,i);
}
