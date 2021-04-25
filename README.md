A simple Compiler for a custom language .The compiler contains a parser , scanner,semantics and 3AC converter.

Run instruction:
    Run this commands in Linux terminal
    
        lex myScanner.l 
        
        yacc myParser.y -d 
        
        cc symbol.c lex.yy.c y.tab.c -o compiler -ll
        
