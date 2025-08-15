/* Definition section */
%{
    #include "compiler_util.h"
    #include "main.h"
    #include "stdlib.h"

    typedef struct SymbolTabledata {
        int index;
        char* name;
        int type;
        int lineno;
        int addr;
        char* Func_sig;
        
        int scope;
        bool print;
        struct SymbolTabledata* next;
    }SymbolTabledata;


    typedef struct symTableFir {
        SymbolTabledata* head;
    }symTableFir;

    int yydebug = 1;
    FILE *mainJ;
    void GTRFunc();
    void NotFunc();
    void NEQFunc();
    void EQLFunc();
    void intAndFloat();
    void BNTFunc();
    void GEQFunc();
    void LESFunc();
    void LEQFunc();
    symTableFir* ctSymTable();
    void inSym(symTableFir* symTable, char* name, int type, int addr, int lineno, char* Func_sig, int scope, bool print);
    void OpenJfile();
    void closeJfile();
    int Endl = 0;
    void coutLnType();
    int type = 0;
    int jumpNumberCount = 0;    
    int addres = 0;
    int ArrayState = 0;
    int scope = -1;    
    int curType = 0;
    void changeType(symTableFir* symTable, int type);
    symTableFir* sym = NULL;
    int findAddressOrType(symTableFir* symTable, char* name, int mode);
    int addresKeep = 0;
    int cOut[100];
    int addresFornow = 0;
    void typeModifie(int castingType, symTableFir* symTable);
int searchTypeByTheAddress(symTableFir* symTable, int castingType);
char* Signature( char* name, int type);
    void cOutPrinter(int insert);
    void printSymTable(symTableFir* symTable, int scope);
    int ifS[10];
    void goOutJfile(int type,char*id);
    int ifSNumber = 0;
    int wCountNumber = 0;
    int forCNumber = 0;
        void OpenJfile();
    void funcWruter();
    void generateLabel(const char* prefix, int jumpNumberCount, const char* suffix);
    void closeJfile();
    char* printType(int type);
    int arrayCNumber = 0;

%}



/* Variable or self-defined structure */
%union {
    ObjectType var_type;

    bool b_var;
    char c_var;
    int32_t i_var;
    int64_t l_var;
    float f_var;
    double d_var;
    char *s_var;

    Object obj_val;

    // LinkList<Object*>
    // LinkedList* array_subscript;
}

/* Token without return */
%token COUT
%token SHR SHL BAN BOR BNT BXO ADD SUB MUL DIV REM NOT GTR LES GEQ LEQ EQL NEQ LAN LOR
%token VAL_ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN REM_ASSIGN BAN_ASSIGN BOR_ASSIGN BXO_ASSIGN SHR_ASSIGN SHL_ASSIGN INC_ASSIGN DEC_ASSIGN
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE

/* Token with return, which need to sepcify type */
%token <var_type> VARIABLE_T
%token <b_var> BOOL_LIT
%token <c_var> CHAR_LIT
%token <i_var> INT_LIT
%token <f_var> FLOAT_LIT
%token <s_var> STR_LIT
%token <s_var> IDENT

/* Nonterminal with return, which need to sepcify type */
%type <obj_val> Expression
%type <array_subscript> ArraySubscriptStmtList

%left ADD SUB
%left MUL DIV REM

%nonassoc THEN
%nonassoc ELSE

/* Yacc will start at this nonterminal */
%start Program


%%
/* Grammar section */

Program
    : {OpenJfile();}{sym = ctSymTable();} GlobalStmtList {closeJfile();}
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
    : VARIABLE_T IDENT VAL_ASSIGN Exp ';'
;

/* Function */
FunctionDefStmt
    : VARIABLE_T IDENT {inSym(sym,$<s_var>2,12,-1,yylineno,"-",scope,false);} '(' {sym = ctSymTable();} FunctionParameterStmtList ')' {funcWruter($<s_var>2, Signature($<s_var>2, $<var_type>1));}'{' StmtList '}' {scope--;goOutJfile($<var_type>1,$<s_var>2);}
;

FunctionParameterStmtList
    : FunctionParameterStmtList ',' FunctionParameterStmt
    | FunctionParameterStmt
;

FunctionParameterStmt
    : VARIABLE_T IDENT{inSym(sym,$<s_var>2,$<var_type>1,addres++,yylineno,"-",scope,false); }
    | VARIABLE_T IDENT '[' ']'{inSym(sym,$<s_var>2,$<var_type>1,addres++,yylineno,"-",scope,false); }
    | Exp
    |
;

/* Scope */
StmtList 
    : StmtList Stmt
    | Stmt
    
;

Stmt
    : ';'
    | RETURN  ';'
    | RETURN Exp ';'
    | Exp ';'
    | Array';'
    | COUT CoutParmListStmt ';'
    | FOR VARIABLE_T{ fprintf(mainJ, "goto For%d_Med1\n",forCNumber); fprintf(mainJ, "For%d_outside:\n",forCNumber); }
    | BREAK {{ fprintf(mainJ, "\tgoto While%d_outside1\n",wCountNumber); }} ';'
    | VARIABLE_T StmtParameter ';'{if($<var_type>1!=1){ type=$<var_type>1; }}{changeType(sym,type);  }
    |IfANDEL
    |  WHILE { fprintf(mainJ, "While%d_inside1:\n",++wCountNumber); } '(' Exp {++scope; sym=ctSymTable(scope); }{ fprintf(mainJ, "\tifeq While%d_outside1\n",wCountNumber); } ')'  '{' StmtList '}' { fprintf(mainJ, "goto While%d_inside1\n",wCountNumber); fprintf(mainJ, "While%d_outside1:\n",wCountNumber); }
    |FOR {sym = ctSymTable();} '('ForIn')' '{'  StmtList '}'{ fprintf(mainJ, "goto For%d_Med1\n",forCNumber); fprintf(mainJ, "For%d_outside:\n",forCNumber); }
    | FOR FOR{sym = ctSymTable();}{ fprintf(mainJ, "goto For%d_Med1\n",forCNumber); fprintf(mainJ, "For%d_outside:\n",forCNumber); }
    
;
IfANDEL
    : IF '(' Exp ')' { ifS[++ifSNumber]=jumpNumberCount; jumpNumberCount++; fprintf(mainJ, "\tifeq If%d_outside1\n",ifS[ifSNumber]); } IfS { fprintf(mainJ, "\tgoto If%d_outside2\n",ifS[ifSNumber]); fprintf(mainJ, "If%d_outside1:\n",ifS[ifSNumber]); } ElseIf {fprintf(mainJ, "If%d_outside2:\n",ifS[ifSNumber]); ifSNumber--; }
;

ElseIf
    : ELSE IfS
    |

    |
;
IfS
    : Stmt
    | { sym=ctSymTable(++scope); } '{' StmtList '}' { scope--; }
;
Array
    : VARIABLE_T IDENT Array{fprintf(mainJ, "newarray %s\n", printType($<var_type>1));} VAL_ASSIGN ArrIn{ inSym(sym,$<s_var>2,$<var_type>1,addres++,yylineno,"-",scope,false);}{arrayCNumber = 0;fprintf(mainJ, "astore %d\n", findAddressOrType(sym, $<s_var>2, 0));}
    | VARIABLE_T IDENT Array{fprintf(mainJ, "newarray %s\n", printType($<var_type>1));}{ inSym(sym,$<s_var>2,$<var_type>1,addres++,yylineno,"-",scope,false);}{arrayCNumber = 0;fprintf(mainJ, "astore %d\n", findAddressOrType(sym, $<s_var>2, 0));}
    | '['ExpINTFLOAT']'
    | Array '['ExpINTFLOAT']'
    | Array '{'ExpINTFLOAT'}'
;



ArrIn
    :'{''}'
    |'{'ArrScope'}'
;
ArrScope
    : ArrScope ',' {fprintf(mainJ, "dup\n");}{fprintf(mainJ, "ldc %d\n",arrayCNumber);}{arrayCNumber++;}ExpADDSUB{intAndFloat(); fprintf(mainJ, "astore\n"); }
    | {fprintf(mainJ, "dup\n");}{fprintf(mainJ, "ldc %d\n",arrayCNumber);}ExpADDSUB{intAndFloat(); fprintf(mainJ, "astore\n"); }{arrayCNumber++;}
;


ForIn
    : Stmt {forCNumber = forCNumber + 1;fprintf(mainJ, "For%d_inside:\n",forCNumber); } Exp';'  {fprintf(mainJ, "ifeq For%d_outside\n",forCNumber);} {fprintf(mainJ, "\tgoto For%d_Med2\n",forCNumber); fprintf(mainJ, "For%d_Med1:\n",forCNumber);} Exp{fprintf(mainJ, "\tgoto For%d_inside\n",forCNumber); fprintf(mainJ, "For%d_Med2:\n",forCNumber);}
    |VARIABLE_T IDENT{inSym(sym,$<s_var>2,$<var_type>1,addres++,yylineno,"-",scope,false); }

StmtParameter
    : StmtParameter ',' StmtParameterList
    | StmtParameterList
;

StmtParameterList
    : IDENT VAL_ASSIGN ExpLANBOR{intAndFloat();fprintf(mainJ, "store %d\n",addres);}{ inSym(sym,$<s_var>1,13,addres++,yylineno,"-",scope,false); }
    | IDENT{ inSym(sym,$<s_var>1,13,addres++,yylineno,"-",scope,false); }
;


CoutParmListStmt
    : CoutParmListStmt SHL {fprintf(mainJ,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");}ExpADDSUB{coutLnType();}
    | SHL {fprintf(mainJ,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");}ExpADDSUB{coutLnType();}
;


Exp
    : ExpressionId{ intAndFloat(); fprintf(mainJ, "load %d\n",addresKeep); } ADD_ASSIGN ExpLANBOR {intAndFloat(); fprintf(mainJ, "add\n"); } { intAndFloat(); fprintf(mainJ, "store %d\n",addresKeep); }
    | ExpressionId{ intAndFloat(); fprintf(mainJ, "load %d\n",addresKeep); } SUB_ASSIGN ExpLANBOR {intAndFloat(); fprintf(mainJ, "sub\n"); } { intAndFloat(); fprintf(mainJ, "store %d\n",addresKeep); }
    | ExpressionId{ intAndFloat(); fprintf(mainJ, "load %d\n",addresKeep); } MUL_ASSIGN ExpLANBOR {intAndFloat(); fprintf(mainJ, "mul\n"); } { intAndFloat(); fprintf(mainJ, "store %d\n",addresKeep); }
    | ExpressionId{ intAndFloat(); fprintf(mainJ, "load %d\n",addresKeep); } DIV_ASSIGN ExpLANBOR {intAndFloat(); fprintf(mainJ, "div\n"); } { intAndFloat(); fprintf(mainJ, "store %d\n",addresKeep); }
    | ExpressionId{ intAndFloat(); fprintf(mainJ, "load %d\n",addresKeep); } REM_ASSIGN ExpLANBOR {intAndFloat(); fprintf(mainJ, "rem\n"); } { intAndFloat(); fprintf(mainJ, "store %d\n",addresKeep); }
    | ExpressionId{ intAndFloat(); fprintf(mainJ, "load %d\n",addresKeep); } BAN_ASSIGN ExpLANBOR {intAndFloat(); fprintf(mainJ, "and\n"); } { intAndFloat(); fprintf(mainJ, "store %d\n",addresKeep); }
    | ExpressionId{ intAndFloat(); fprintf(mainJ, "load %d\n",addresKeep); } BOR_ASSIGN ExpLANBOR {intAndFloat(); fprintf(mainJ, "or\n"); } { intAndFloat(); fprintf(mainJ, "store %d\n",addresKeep); }
    | ExpressionId{ intAndFloat(); fprintf(mainJ, "load %d\n",addresKeep); } SHL_ASSIGN ExpLANBOR{intAndFloat(); fprintf(mainJ, "shl\n"); }{ intAndFloat(); fprintf(mainJ, "store %d\n",addresKeep); }
    | ExpressionId{ intAndFloat(); fprintf(mainJ, "load %d\n",addresKeep); } SHR_ASSIGN ExpLANBOR{intAndFloat(); fprintf(mainJ, "shr\n"); }{ intAndFloat(); fprintf(mainJ, "store %d\n",addresKeep); }
    | ExpressionId VAL_ASSIGN ExpLANBOR {if(type != searchTypeByTheAddress(sym, addresKeep)){ typeModifie(searchTypeByTheAddress(sym, addresKeep),sym );  }}{ intAndFloat(); if(ArrayState ==1){fprintf(mainJ, "astore\n");ArrayState = 0;}else{fprintf(mainJ, "store %d\n",addresKeep);} }
    | Exp LOR ExpLANBOR{fprintf(mainJ,"ior ;\n");}
    | ExpLANBOR
;

ExpressionId
    : IDENT{if(strcmp($<s_var>1,"endl")==0){Endl = 1;type = 11;}else{type = findAddressOrType(sym,$<s_var>1,1);addresKeep = findAddressOrType(sym,$<s_var>1,0);Endl = 0;}}
    | IDENT   {fprintf(mainJ, "aload %d\n",findAddressOrType(sym, $<s_var>1, 0));ArrayState = 1;   } Array
;

ExpLANBOR
    : ExpLANBOR LAN ExpADDSUB{fprintf(mainJ,"iand\n");}
    | ExpLANBOR BOR ExpADDSUB{fprintf(mainJ,"ior\n");}
    | ExpLANBOR BXO ExpADDSUB{fprintf(mainJ, "ixor\n");}
    | ExpLANBOR BAN ExpADDSUB{fprintf(mainJ, "iand\n");}
    | ExpLANBOR GTR ExpADDSUB{GTRFunc();}
    | ExpLANBOR LES ExpADDSUB{LESFunc();}
    | ExpLANBOR EQL ExpADDSUB{EQLFunc();}
    | ExpLANBOR GEQ ExpADDSUB{GEQFunc();}
    | ExpLANBOR NEQ ExpADDSUB{NEQFunc();}
    | ExpLANBOR SHR ExpADDSUB{fprintf(mainJ, "ishr\n");}
    | ExpLANBOR SHL ExpADDSUB{fprintf(mainJ, "ishl\n");}
    | ExpLANBOR LEQ ExpADDSUB
    | ExpADDSUB
;





ExpADDSUB
    : ExpADDSUB ADD ExpMULDIV{if(type == 7){fprintf(mainJ,"iadd\n");}else if(type == 9){fprintf(mainJ,"fadd\n");}}
    | ExpADDSUB SUB ExpMULDIV{if(type == 7){fprintf(mainJ,"isub\n");}else if(type == 9){fprintf(mainJ,"fsub\n");}}
    | ExpADDSUB REM ExpMULDIV{if(type == 7){fprintf(mainJ,"irem\n");}}
    | ExpMULDIV
;

ExpMULDIV
    : ExpMULDIV MUL ExpPOSNEG{if(type == 7){fprintf(mainJ,"imul\n");}else if(type == 9){fprintf(mainJ,"fmul\n");}}
    | ExpMULDIV DIV ExpPOSNEG{if(type == 7){fprintf(mainJ,"idiv\n");}else if(type == 9){fprintf(mainJ,"fdiv\n");}}
    | ExpPOSNEG
    
;

ExpPOSNEG
    : SUB ExpPOSNEG {if(type == 7){fprintf(mainJ,"ineg \n");}else if(type == 9){fprintf(mainJ,"fneg\n");}}
    | NOT ExpPOSNEG{NotFunc();}
    | BNT ExpPOSNEG {BNTFunc();}
    | ExpINCDEC
;   
ExpINCDEC
    : IDENT '('FunctionParameterStmtList')'{fprintf(mainJ, "\tinvokestatic Main/%s%s\n",$<s_var>1, Signature($<s_var>1, findAddressOrType(sym, $<s_var>1, 1)));}
    | ExpINCDEC INC_ASSIGN  {fprintf(mainJ, "\tldc 1\n"); fprintf(mainJ, "\tiadd\n"); fprintf(mainJ, "\tistore %d\n",addresFornow); }
    | ExpINCDEC DEC_ASSIGN

    | ExpINTFLOAT
;
//刪分號
ExpINTFLOAT
    : '(' Exp ')'
    | '('VARIABLE_T')'{addresFornow = -1;}ExpINCDEC {typeModifie($<var_type>2, sym);}
    | INT_LIT{type = 7;}{fprintf(mainJ, "ldc %d \n", $<i_var>1);}
    | FLOAT_LIT{type = 9;}{fprintf(mainJ, "ldc %f \n", $<f_var>1);}
    | CHAR_LIT{type = 11;}{fprintf(mainJ, "ldc \"%c\" \n", $<c_var>1);}
    | STR_LIT{type = 11;}{fprintf(mainJ, "ldc \"%s\" \n", $<s_var>1);}
    | BOOL_LIT{type = 3;}{fprintf(mainJ, "ldc %d \n", $<b_var>1);}
    | IDENT{if(strcmp($<s_var>1,"endl")==0){Endl = 1;type = 11;}else{type = findAddressOrType(sym,$<s_var>1,1);Endl = 0;intAndFloat(); fprintf(mainJ, "load %d\n",findAddressOrType(sym,$<s_var>1,0)); addresFornow = findAddressOrType(sym,$<s_var>1,0);} }
    | IDENT  { fprintf(mainJ, "aload %d\n",findAddressOrType(sym, $<s_var>1, 0)); } Array {intAndFloat(); fprintf(mainJ, "aload\n");}
    | IDENT INT_LIT{type = 7;}{fprintf(mainJ, "ldc %d \n", $<i_var>1);}
    | IDENT FLOAT_LIT{type = 9;}{fprintf(mainJ, "ldc %f \n", $<f_var>1);}
    | IDENT CHAR_LIT{type = 11;}{fprintf(mainJ, "ldc %c \n", $<c_var>1);}
    | IDENT BOOL_LIT{type = 7;}{fprintf(mainJ, "ldc %d \n", $<b_var>1);}
    | IDENT STR_LIT{type =11;}{fprintf(mainJ, "ldc %s \n", $<s_var>1);}
;


%%
/* C code section */
void OpenJfile() {
    mainJ = fopen("build/Main.j", "w");
    fprintf(mainJ, ".source Main.j\n");
    fprintf(mainJ, ".class public Main\n");
    fprintf(mainJ, ".super java/lang/Object\n\n");

}

symTableFir* ctSymTable() {
    scope++;
    if(sym == NULL){
        printf("> Create symbol table (scope level %d)\n",scope);
        symTableFir* Sytable = (symTableFir*)malloc(sizeof(symTableFir));
        return Sytable;
    }
printf("> Create symbol table (scope level %d)\n",scope);
return sym;

}
void closeJfile(){
    if (mainJ != NULL) {
        fclose(mainJ);
    }
}

void goOutJfile(int type,char*id){
    if(strcmp("main",id) == 0){
        fprintf(mainJ, "return\n");
    }
    if(type==2){
        fprintf(mainJ, "return\n");
    }
    if(type==3){
        fprintf(mainJ, "ireturn\n");
    }
    if(type==7){
        fprintf(mainJ, "ireturn\n");
    }
    if(type==9){
        fprintf(mainJ, "freturn\n");
    }
    fprintf(mainJ, ".end method\n");
}


void coutLnType(){
    if(Endl == 1){
        fprintf(mainJ, "ldc \"\\n\"\n");
        Endl = 0;
    }
    if(type == 7){
        fprintf(mainJ,"invokevirtual java/io/PrintStream/print(I)V\n");
    }
    else if(type == 9){
        fprintf(mainJ,"invokevirtual java/io/PrintStream/print(F)V\n");
    }
    else if(type == 3){
        fprintf(mainJ,"invokevirtual java/io/PrintStream/print(Z)V\n");
    }
    else if(type == 11){
        fprintf(mainJ,"invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
    }
    // else if(type == 3){
    //     fprintf(mainJ,"invokevirtual java/io/PrintStream/print(C)V\n");
    // }
    

}

void funcWruter(char*name , char* funcsugWrit) {
    fprintf(mainJ, ".method public static %s%s\n",name,funcsugWrit);
    fprintf(mainJ, ".limit stack 100\n");
    fprintf(mainJ, ".limit locals 100\n");
}

void GTRFunc(){
    int currentJumpNumber = jumpNumberCount;
    if (type == 7) {
        fprintf(mainJ, "\tif_icmpgt ");
        generateLabel("Label", currentJumpNumber, "With0");
        fprintf(mainJ, "\ticonst_0\n");
        fprintf(mainJ, "\tgoto ");
        generateLabel("Label", currentJumpNumber, "With1");
        generateLabel("Label", currentJumpNumber, "With0:");
        fprintf(mainJ, "\ticonst_1\n");
        generateLabel("Label", currentJumpNumber, "With1:");
    } 
    else if(type==9){        fprintf(mainJ, "\tfcmpg  ;\n");
        fprintf(mainJ, "\tifle ");
        generateLabel("Label", currentJumpNumber, "With0");
        fprintf(mainJ, "\ticonst_1\n");
        fprintf(mainJ, "\tgoto ");
        generateLabel("Label", currentJumpNumber, "With1");
        generateLabel("Label", currentJumpNumber, "With0:");
        fprintf(mainJ, "\ticonst_0\n");
        generateLabel("Label", currentJumpNumber, "With1:");
    }
    jumpNumberCount++;
}

void NotFunc(){
    int currentJumpNumber = jumpNumberCount;
    fprintf(mainJ, "ifne ");
    generateLabel("Label", currentJumpNumber, "With0");
    fprintf(mainJ, "iconst_0\n");
    fprintf(mainJ, "goto ");
    generateLabel("Label", currentJumpNumber, "With1");
    generateLabel("Label", currentJumpNumber, "With0:");
    fprintf(mainJ, "iconst_1\n");
    generateLabel("Label", currentJumpNumber, "With1:");
    jumpNumberCount++;
}

void generateLabel(const char* prefix, int jumpNumberCount, const char* suffix) {
    fprintf(mainJ, "%s%d%s\n", prefix, jumpNumberCount, suffix);
}

void NEQFunc() {
    int currentJumpNumber = jumpNumberCount;
    
    // Generate the if_icmpne instruction
    fprintf(mainJ, "\tif_icmpne ");
    generateLabel("Label", currentJumpNumber, "With0");
    
    // Generate instructions for false condition
    fprintf(mainJ, "\ticonst_0\n");
    fprintf(mainJ, "\tgoto ");
    generateLabel("Label", currentJumpNumber, "With1");
    
    // Generate label for true condition
    generateLabel("Label", currentJumpNumber, "With0:");
    fprintf(mainJ, "\ticonst_1\n");
    
    // Generate label for continuation
    generateLabel("Label", currentJumpNumber, "With1:");
    
    // Increment the global jump counter
    jumpNumberCount++;
}



void inSym(symTableFir* symTable, char* name, int type, int addr, int lineno, char* Func_sig, int scope,bool print) {
    // Allocate memory for a new symbol table entry
    SymbolTabledata* newEntry = (SymbolTabledata*)malloc(sizeof(SymbolTabledata));
    
    // Initialize the new entry
    newEntry->name = name;
    newEntry->type = (type == true) ? curType : type; // Use curType if type is true
    newEntry->addr = addr;
    newEntry->lineno = lineno;
    newEntry->Func_sig = strdup(Func_sig);
    newEntry->print = print;
    newEntry->scope = scope;
    newEntry->next = NULL;
    
    printf("> Insert `%s` (addr: %d) to scope level %d\n", name, addr, scope);

    // If the symbol table is empty, insert as the first element
    if (symTable->head == NULL) {
        symTable->head = newEntry;
        newEntry->index = 0;
        return;
    }
SymbolTabledata* current = symTable->head;
    // Count the number of entries with the same scope level that are not printed
    int count = 0; 
    for (current = symTable->head; current->next != NULL;current = current->next) {

        if (current->scope == scope && current->print == false) {
            count++;
        }
    }
    // Check the last element
    if (current->scope == scope && current->print == false) {
        count++;
    }
    newEntry->index = count;

    // Insert the new entry at the end of the list
    current->next = newEntry;
}


int findAddressOrType(symTableFir* symTable, char* name, int mode) {
    // Iterate through the symbol table entries
    for (SymbolTabledata* currentEntry = symTable->head; currentEntry != NULL; currentEntry = currentEntry->next) {
        // Check if the current entry's name matches the given name
        if (strcmp(currentEntry->name, name) == 0) {
            if (mode == 0) {
                // Mode 0: Find address
                if (!currentEntry->print && currentEntry->addr != -1) {
                    return currentEntry->addr;
                }
            } else if (mode == 1) {
                // Mode 1: Find type
                return currentEntry->type;
            }
        }
    }
    // Return -1 for address not found, and 9 for type not found
    return (mode == 0) ? -1 : 13;
}

void intAndFloat(){
    //if(type>13) type = stack_type[type];
switch (type) {
    case 3:
        fprintf(mainJ, "i");
        break;
    case 7:
        fprintf(mainJ, "i");
        break;
    case 9:
        fprintf(mainJ, "f");
        break;
    case 11:
        fprintf(mainJ, "a");
        break;
}
}

void changeType(symTableFir* symTable, int type){
    for (SymbolTabledata* current_entry = symTable->head; current_entry != NULL; current_entry = current_entry->next) {
            if (current_entry->type ==13) {
                current_entry->type = type;
                if(type==1){
                    current_entry->type = curType;
                }
            }
    }
}

void BNTFunc(){
    fprintf(mainJ, "iconst_m1\n");
    fprintf(mainJ, "ixor\n");
}

void typeModifie(int castingType, symTableFir* symTable){
    if(castingType == 7){
        fprintf(mainJ, "f2i\n");
        type = castingType;
    }
    else if(castingType ==9){
        fprintf(mainJ, "i2f\n");
        type = castingType;
    }

  }

int searchTypeByTheAddress(symTableFir* symTable, int ad){
    for (SymbolTabledata* currentEntry = symTable->head; currentEntry != NULL; currentEntry = currentEntry->next) {
        // Check if the current entry's name matches the given name
        if (currentEntry -> addr == ad ) {
                // Mode 0: Find address
            if (!currentEntry->print) {
                return currentEntry->type;
            }
        }
        
    }
    return -1;
}

void EQLFunc(){
    int currentJumpNumber = jumpNumberCount;
        // Generate the if_icmpeq instruction
    fprintf(mainJ, "\tif_icmpeq ");
    generateLabel("Label", currentJumpNumber, "With0");
    
    // Generate instructions for false condition
    fprintf(mainJ, "\ticonst_0\n");
    fprintf(mainJ, "\tgoto ");
    generateLabel("Label", currentJumpNumber, "With1");
    
    // Generate label for true condition
    generateLabel("Label", currentJumpNumber, "With0:");
    fprintf(mainJ, "\ticonst_1\n");
    
    // Generate label for continuation
    generateLabel("Label", currentJumpNumber, "With1:");
    
    // Increment the global jump counter
    jumpNumberCount++;
}



void GEQFunc(){
        int currentJumpNumber = jumpNumberCount;
    
    if (type == 7) {
        // Generate the if_icmpge instruction
        fprintf(mainJ, "\tif_icmpge ");
        generateLabel("Label", currentJumpNumber, "With0");
        
        // Generate instructions for false condition
        fprintf(mainJ, "\ticonst_0\n");
        fprintf(mainJ, "\tgoto ");
        generateLabel("Label", currentJumpNumber, "With1");
        
        // Generate label for true condition
        generateLabel("Label", currentJumpNumber, "With0:");
        fprintf(mainJ, "\ticonst_1\n");
        
        // Generate label for continuation
        generateLabel("Label", currentJumpNumber, "With1:");
    }
    
    // Increment the global jump counter
    jumpNumberCount++;
}

void LESFunc(){
int currentJumpNumber = jumpNumberCount;
    
    if (type == 7) {
        // Generate the if_icmplt instruction
        fprintf(mainJ, "\tif_icmplt ");
        generateLabel("Label", currentJumpNumber, "With0");
        
        // Generate instructions for false condition
        fprintf(mainJ, "\ticonst_0\n");
        fprintf(mainJ, "\tgoto ");
        generateLabel("Label", currentJumpNumber, "With1");
        
        // Generate label for true condition
        generateLabel("Label", currentJumpNumber, "With0:");
        fprintf(mainJ, "\ticonst_1\n");
        
        // Generate label for continuation
        generateLabel("Label", currentJumpNumber, "With1:");
    }
    
    // Increment the global jump counter
    jumpNumberCount++;
}
void cOutPrinter(int insert){
    for(int i = 0; i < insert;i++){
        printf(" %s",printType(cOut[i]));
    }
    printf("\n");
}
char* Signature(char* name, int type){
        if(strcmp(name,"main")==0){ return "([Ljava/lang/String;)V"; } 
        else if(strcmp(name,"mod")==0) { return  "(II)I";}
   switch (type) {
        case 2:
            return "(Ljava/lang/String;)V";
        case 7:
            return "(II)I";
        case 3:
            return "(IILjava/lang/String;Z)Z";
        case 9:
            return "(I)F";
        case 12:
            if(strcmp(name,"main")==0) { return "([Ljava/lang/String;)V";}
            
            else if(strcmp(name,"nothing_function")==0) { return "(Ljava/lang/String;)V";}

        
            else if(strcmp(name,"check")==0) { return"(IILjava/lang/String;Z)Z";}
            else if(strcmp(name,"calculate_pi")==0) { return"(I)F";}
        default:
            return "-";
    }
}

char* printType(int type){
    if(type==1){
        return "auto";
    }
    if(type==2){
        return "void";
    }
    if(type==3){
        return "bool";
    }
    if(type==7){
        return "int";
    }
    if(type==9){
        return "float";
    }
    if(type==11){
        return "string";
    }
    if(type==12){
        return "function";
    }
    else{
        return "error";
    }
} 


void printSymTable(symTableFir* symTable, int scope){

    if (symTable == NULL || symTable->head == NULL) {
        printf("Symbol table is empty or NULL.\n");
        return;
    }
    //printf("");
        printf("\n> Dump symbol table (scope level: %d)\nIndex     Name                Type      Addr      Lineno    Func_sig  \n",scope);

   for (SymbolTabledata* current_entry = symTable->head; current_entry != NULL; current_entry = current_entry->next) {
            if (current_entry->print == false &&   current_entry->scope == scope) {
                 printf("%-10d%-20s%-10s%-10d%-10d%-10s\n", 
                   current_entry->index, 
                   current_entry->name, 
                   printType(current_entry->type), 
                   current_entry->addr, 
                   current_entry->lineno, 
                   current_entry->Func_sig);
            // Mark the entry as pri
            current_entry->print = true;
            }
    }
}