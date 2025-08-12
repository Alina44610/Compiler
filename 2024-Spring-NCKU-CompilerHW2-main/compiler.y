/* Definition section */
%{
    #include "compiler_common.h"
    #include "compiler_util.h"
    #include "main.h"

    int yydebug = 1;
    int scope = -1;
    int addr = -2;
    int addr_no = VAR_FLAG_DEFAULT;
    ObjectType temp;


    typedef struct SymbolTableEntry {
        int index;
        char* name;
        int type;
        int addr;
        int lineno;
        char* Func_sig;

        int scope_level;
        struct SymbolTableEntry* next;
    }SymbolTableEntry;

    typedef struct SymbolTable {
        SymbolTableEntry* head;
    }SymbolTable;

    SymbolTable* st = NULL;
    SymbolTable* createSymbolTable();
    void insertSymbol(SymbolTable* symbolTable, char* name, int type, int addr, int lineno, char* Func_sig, int scope_level);
    void printSymbolTable(SymbolTable* symbolTable, int scope_level);
%}

/* Variable or self-defined structure */
%union {
    ObjectType var_type;

    bool b_var;
    int i_var;
    float f_var;
    char *s_var;

    Object object_var;
}

/* Token without return */
%token COUT
%token INT FLOOT BOOL STR
%token SHR SHL BAN BOR BNT BXO ADD SUB MUL DIV REM NOT GTR LES GEQ LEQ EQL NEQ LAN LOR
%token VAL_ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN REM_ASSIGN BAN_ASSIGN BOR_ASSIGN BXO_ASSIGN SHR_ASSIGN SHL_ASSIGN INC_ASSIGN DEC_ASSIGN
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE

/* Token with return, which need to sepcify type */
%token <var_type> VARIABLE_T
%token <s_var> IDENT
%token <i_var> INT_LIT
%token <b_var> BOOL_LIT
%token <f_var> FLOAT_LIT
%token <s_var> STR_LIT

/* Nonterminal with return, which need to sepcify type */
%type <object_val> Expression

%left ADD SUB
%left MUL DIV REM

/* Yacc will start at this nonterminal */
%start Program

%%
/* Grammar section */

Program
    : { pushScope(); st = createSymbolTable();} GlobalStmtList { dumpScope(); } {printSymbolTable(st, scope);scope--;}
;

GlobalStmtList 
    : GlobalStmtList GlobalStmt
    | GlobalStmt
;

GlobalStmt
    : DefineVariableStmt
    | FunctionDefStmt
;

DefineVariableStmt
    : VARIABLE_T IDENT VAL_ASSIGN Expression ';'
;

/* Function */
FunctionDefStmt
    : VARIABLE_T IDENT { printf("func: %s\n",$<s_var>2);} {insertSymbol(st,$<s_var>2,$<var_type>1,333,yylineno,"-",scope);} '(' { st = createSymbolTable(); } FunctionParameterStmtList ')' { createFunction($<var_type>1, $<s_var>2); } '{' StmtList '}' { dumpScope(); }{printSymbolTable(st, scope);scope--;}
;

FunctionParameterStmtList
    : FunctionParameterStmtList ',' FunctionParameterStmt
    | FunctionParameterStmt
;

FunctionParameterStmt
    : VARIABLE_T IDENT {insertSymbol(st,$<s_var>2,$<var_type>1,333,yylineno,"-",scope); }{ pushFunParm($<var_type>1, $<s_var>2, VAR_FLAG_DEFAULT); }
    | VARIABLE_T IDENT '[' ']' {insertSymbol(st,$<s_var>2,$<var_type>1,555,yylineno,"-",scope); }{ pushFunParm($<var_type>1, $<s_var>2, VAR_FLAG_DEFAULT); }
    | Expression
;

/* Scope */
StmtList 
    : StmtList Stmt
    | Stmt
    
;

Stmt
    : ';'
    | Expression ';'
    | COUT CoutParmListStmt ';' { printf("cout\n"); } { stdoutPrint(); }
    | RETURN Expression ';' { printf("RETURN\n"); }
    | VARIABLE_T StmtParameter ';'
    | IF '('Expression')' '{' {printf("IF\n");}{ st = createSymbolTable(); } StmtList '}'{ dumpScope(); }{printSymbolTable(st, scope);scope--;}
    | ELSE '{'{printf("ELSE\n");}{ st = createSymbolTable(); } StmtList '}'{ dumpScope(); }{printSymbolTable(st, scope);scope--;}
    | WHILE {printf("WHILE\n");}'('Expression')' '{'{ st = createSymbolTable(); } StmtList '}'{ dumpScope(); }{printSymbolTable(st, scope);scope--;}
    | FOR {printf("FOR\n");} { st = createSymbolTable(); }'('Stmt Expression';' Expression')' '{'  StmtList '}'{ dumpScope(); }{printSymbolTable(st, scope);scope--;}
;

StmtParameter
    : StmtParameter ',' StmtParameterList
    | StmtParameterList
;

StmtParameterList
    : IDENT VAL_ASSIGN ExpressionAdd { insertSymbol(st,$<s_var>1,11,667,yylineno,"-",scope); }
    | IDENT { insertSymbol(st,$<s_var>1,11,668,yylineno,"-",scope); }
;

CoutParmListStmt
    : CoutParmListStmt SHL ExpressionAdd
    | SHL ExpressionAdd
;

Expression
    : IDENT {printf("IDENT (name=%s, address=9)\n",$<s_var>1);} ADD_ASSIGN ExpressionAndand {printf("ADD_ASSIGN\n");}
    | IDENT {printf("IDENT (name=%s, address=2)\n",$<s_var>1);} SUB_ASSIGN ExpressionAndand {printf("SUB_ASSIGN\n");}
    | IDENT {printf("IDENT (name=%s, address=3)\n",$<s_var>1);} MUL_ASSIGN ExpressionAndand {printf("MUL_ASSIGN\n");}
    | IDENT {printf("IDENT (name=%s, address=4)\n",$<s_var>1);} DIV_ASSIGN ExpressionAndand {printf("DIV_ASSIGN\n");}
    | IDENT {printf("IDENT (name=%s, address=5)\n",$<s_var>1);} REM_ASSIGN ExpressionAndand {printf("REM_ASSIGN\n");}
    | IDENT {printf("IDENT (name=%s, address=6)\n",$<s_var>1);} BAN_ASSIGN ExpressionAndand {printf("BAN_ASSIGN\n");}
    | IDENT {printf("IDENT (name=%s, address=7)\n",$<s_var>1);} BOR_ASSIGN ExpressionAndand {printf("BOR_ASSIGN\n");}
    | IDENT {printf("IDENT (name=%s, address=8)\n",$<s_var>1);} VAL_ASSIGN ExpressionAndand {printf("EQL_ASSIGN\n");}
    | IDENT {printf("IDENT (name=%s, address=9)\n",$<s_var>1);} SHL_ASSIGN ExpressionAndand {printf("SHL_ASSIGN\n");}
    | IDENT {printf("IDENT (name=%s, address=10)\n",$<s_var>1);} SHR_ASSIGN ExpressionAndand {printf("SHR_ASSIGN\n");}
    | Expression LOR ExpressionAndand { printf("LOR\n"); }
    | ExpressionAndand { pushFunInParm(&$<object_var>1); }
;

ExpressionAndand
    : ExpressionAndand LAN ExpressionOr { printf("LAN\n"); }
    | ExpressionOr { pushFunInParm(&$<object_var>1); }
;

ExpressionOr
    : ExpressionOr BOR ExpressionAnd { printf("BOR\n"); }
    | ExpressionXor { pushFunInParm(&$<object_var>1); }
;

ExpressionXor
    : ExpressionXor BXO ExpressionAnd { printf("BXO\n"); }
    | ExpressionAnd { pushFunInParm(&$<object_var>1); }
;

ExpressionAnd
    : ExpressionAnd BAN ExpressionEqual { printf("BAN\n"); }
    | ExpressionEqual { pushFunInParm(&$<object_var>1); }
;

ExpressionEqual
    : ExpressionEqual EQL ExpressionCompare { printf("EQL\n"); }
    | ExpressionEqual NEQ ExpressionCompare { printf("NEQ\n"); }
    | ExpressionCompare { pushFunInParm(&$<object_var>1); }
;

ExpressionCompare
    : ExpressionCompare GTR ExpressionShift { printf("GTR\n"); }
    | ExpressionCompare LES ExpressionShift { printf("LES\n"); }
    | ExpressionCompare GEQ ExpressionShift { printf("GEQ\n"); }
    | ExpressionCompare LEQ ExpressionShift { printf("LEQ\n"); }
    | ExpressionShift { pushFunInParm(&$<object_var>1); }
;

ExpressionShift
    : ExpressionShift SHR ExpressionAdd { printf("SHR\n"); }
    | ExpressionShift SHL ExpressionAdd { printf("SHL\n"); }
    | ExpressionAdd { pushFunInParm(&$<object_var>1); }
;
ExpressionAdd
    : ExpressionAdd ADD ExpressionMulti { printf("ADD\n"); }
    | ExpressionAdd SUB ExpressionMulti { printf("SUB\n"); }
    | ExpressionMulti { pushFunInParm(&$<object_var>1); }
;

ExpressionMulti
    : ExpressionMulti MUL ExpressionUnary { printf("MUL\n"); } { pushFunInParm(&$<object_var>3); }
    | ExpressionMulti DIV ExpressionUnary { printf("DIV\n"); } { pushFunInParm(&$<object_var>3); }
    | ExpressionUnary { pushFunInParm(&$<object_var>1); }
    | ExpressionMulti REM ExpressionUnary { printf("REM\n"); } { pushFunInParm(&$<object_var>3); }
;

ExpressionUnary
    : ADD ExpressionUnary { printf("ADD\n"); }
    | SUB ExpressionUnary { printf("NEG\n"); }
    | NOT ExpressionUnary { printf("NOT\n"); }
    | '('VARIABLE_T')'ExpressionVariable { printf("Cast to %d\n", $<var_type>2); }
    | BNT ExpressionUnary { printf("BNT\n"); }
    | ExpressionINC { pushFunInParm(&$<object_var>1); }
;   
ExpressionINC
    : ExpressionVariable '('FunctionParameterStmtList')'{printf("c\n");}
    | ExpressionINC INC_ASSIGN { printf("INC_ASSIGN\n");}
    | ExpressionINC DEC_ASSIGN { printf("DEC_ASSIGN\n");}
    | ExpressionVariable{ pushFunInParm(&$<object_var>1); }
;

ExpressionVariable
    : IDENT { printf("IDENT (name=%s, address=-993)\n",$<s_var>1); }
    | INT_LIT { printf("INT_LIT %d\n",$<i_var>1); }
    | FLOAT_LIT { printf("FLOAT_LIT %f\n",$<f_var>1); }
    | BOOL_LIT {if($<b_var>1 == 1) {printf("BOOL_LIT TRUE\n");} else {printf("BOOL_LIT FALSE\n");}}
    | STR_LIT { printf("STR_LIT \"%s\"\n",$<s_var>1); }
    | '(' Expression ')'
;

%%
/* C code section */
SymbolTable* createSymbolTable() {
    scope++;
    printf("> Create symbol table (scope level %d)\n",scope);
    if(st == NULL){
        SymbolTable* table = (SymbolTable*)malloc(sizeof(SymbolTable));
        if (table != NULL) {
            table->head = NULL;
        }
        return table;
    }
    else{
        return st;
    }
}

void insertSymbol(SymbolTable* symbolTable, char* name, int type, int addr, int lineno, char* Func_sig, int scope_level) {
    printf("> Insert %s (addr: %d) to scope level %d\n",name,addr,scope);
    // 創建新的符號表項目
    SymbolTableEntry* entry = (SymbolTableEntry*)malloc(sizeof(SymbolTableEntry)); //要空間
    if (entry == NULL) {
        // 如果內存分配失敗，則返回
        printf("Memory allocation failed for symbol table entry\n");
        return;
    }

    // 設置符號表項目的屬性
    entry->name = strdup(name);
    entry->type = type;
    entry->addr = addr;
    entry->lineno = lineno;
    entry->Func_sig = strdup(Func_sig);
    entry->scope_level = scope_level;
    entry->next = NULL;

    // 將符號表項目插入到符號表中
    if (symbolTable->head == NULL) {
        // 如果符號表是空的，將新項目設置為頭指針
        symbolTable->head = entry;
    } else {
        SymbolTableEntry* current_entry = symbolTable->head;
        int count = 0; // 計數器，用於計算相同scope_level的符號數量
        while (current_entry != NULL) {
            if (current_entry->scope_level == scope_level) {
                count++; // 如果找到相同作用域層級的符號，則計數器加一
            }
            current_entry = current_entry->next;
        }
        entry->index = count; // 將index依照相同scope_level來更新count

        // 否則將新項目插入到符號表的末尾
        SymbolTableEntry* current = symbolTable->head;
        while (current->next != NULL) {
            current = current->next;
        }
        current->next = entry;
    }
}

void printSymbolTable(SymbolTable* symbolTable, int scope_level){

    printf("\n");
    printf("> Dump symbol table (scope level: %d)\n",scope_level);
    printf("Index     Name                Type      Addr      Lineno    Func_sig  \n");
    
    SymbolTableEntry* current_entry = symbolTable->head; //從頭開始把相同scope_level的都印出來
    while (current_entry != NULL) {
            if (current_entry->scope_level == scope_level) {
                printf("%-10d%-20s%-10d%-10d%-10d%-10s\n", current_entry->index, current_entry->name, current_entry->type, current_entry->addr, current_entry->lineno, current_entry->Func_sig);
            }
            current_entry = current_entry->next;
        }
}
