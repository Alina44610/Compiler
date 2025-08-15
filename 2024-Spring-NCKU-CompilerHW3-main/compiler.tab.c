/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 2 "compiler.y"

    #include "compiler_util.h"
    #include "main.h"
    #include "stdlib.h"
    #include "string.h"

    typedef struct SymbolTColumn{
        int index;
        char* name;
        int type;
        int addr;
        int lineno;
        char* func_sig;
        int scope_level;
        int print;
        struct SymbolTColumn* next;
    }SymbolTColumn;

    typedef struct SymbolT{
        SymbolTColumn* head;
    }SymbolT;

    int yydebug = 1;
    int address = 0;
    int scope = -1;
    int type_no = 13;
    int println = 0;
    int cur_value_type = 1;
    int store_addr = 0;
    int cur_addr = 0;
    int lineType = 0;
    int compare_count = 0;
    int while_count = 0;
    int for_count = 0;
    int stack_type[30];
    int func_sig_array[30];
    int func_sig_counter = 0;
    int tempFuncSig_counter = 0;
    int ifElseStack[10];
    int ifElseStack_counter = 0;
    char tempFuncSig[10][80];
    SymbolT* syT = NULL;
    SymbolTColumn* tailColumn = NULL;
    FILE *jfile;

    void init_jfile();
    void writeFunc_jfile();
    void close_jfile();
    SymbolT* crt_SymbolT(int insertScope);
    void ins_SymbolT(int type,char* name,int addr,int lineno,char* func_sig,int currentScope);
    char* find_call(char* id);
    int find_index(int currentScope);
    int find_ident_type(char* id);
    int find_ident_address(char* id);
    int findTypeWithAddr(int address);
    void ins_func_sig(char *id);
    void construct_func();
    void print_cout();
    void iORf();
    void opGTR();
    void opGEQ();
    void opLES();
    void opNOT();
    void opNEQ();
    void opBNT();
    void opEQL();
    void casting(int type);

#line 140 "compiler.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    COUT = 258,                    /* COUT  */
    SHR = 259,                     /* SHR  */
    SHL = 260,                     /* SHL  */
    BAN = 261,                     /* BAN  */
    BOR = 262,                     /* BOR  */
    BNT = 263,                     /* BNT  */
    BXO = 264,                     /* BXO  */
    ADD = 265,                     /* ADD  */
    SUB = 266,                     /* SUB  */
    MUL = 267,                     /* MUL  */
    DIV = 268,                     /* DIV  */
    REM = 269,                     /* REM  */
    NOT = 270,                     /* NOT  */
    GTR = 271,                     /* GTR  */
    LES = 272,                     /* LES  */
    GEQ = 273,                     /* GEQ  */
    LEQ = 274,                     /* LEQ  */
    EQL = 275,                     /* EQL  */
    NEQ = 276,                     /* NEQ  */
    LAN = 277,                     /* LAN  */
    LOR = 278,                     /* LOR  */
    VAL_ASSIGN = 279,              /* VAL_ASSIGN  */
    ADD_ASSIGN = 280,              /* ADD_ASSIGN  */
    SUB_ASSIGN = 281,              /* SUB_ASSIGN  */
    MUL_ASSIGN = 282,              /* MUL_ASSIGN  */
    DIV_ASSIGN = 283,              /* DIV_ASSIGN  */
    REM_ASSIGN = 284,              /* REM_ASSIGN  */
    BAN_ASSIGN = 285,              /* BAN_ASSIGN  */
    BOR_ASSIGN = 286,              /* BOR_ASSIGN  */
    BXO_ASSIGN = 287,              /* BXO_ASSIGN  */
    SHR_ASSIGN = 288,              /* SHR_ASSIGN  */
    SHL_ASSIGN = 289,              /* SHL_ASSIGN  */
    INC_ASSIGN = 290,              /* INC_ASSIGN  */
    DEC_ASSIGN = 291,              /* DEC_ASSIGN  */
    IF = 292,                      /* IF  */
    ELSE = 293,                    /* ELSE  */
    FOR = 294,                     /* FOR  */
    WHILE = 295,                   /* WHILE  */
    RETURN = 296,                  /* RETURN  */
    BREAK = 297,                   /* BREAK  */
    CONTINUE = 298,                /* CONTINUE  */
    VARIABLE_T = 299,              /* VARIABLE_T  */
    BOOL_LIT = 300,                /* BOOL_LIT  */
    CHAR_LIT = 301,                /* CHAR_LIT  */
    INT_LIT = 302,                 /* INT_LIT  */
    FLOAT_LIT = 303,               /* FLOAT_LIT  */
    STR_LIT = 304,                 /* STR_LIT  */
    IDENT = 305,                   /* IDENT  */
    THEN = 306                     /* THEN  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 72 "compiler.y"

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

#line 255 "compiler.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);



/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_COUT = 3,                       /* COUT  */
  YYSYMBOL_SHR = 4,                        /* SHR  */
  YYSYMBOL_SHL = 5,                        /* SHL  */
  YYSYMBOL_BAN = 6,                        /* BAN  */
  YYSYMBOL_BOR = 7,                        /* BOR  */
  YYSYMBOL_BNT = 8,                        /* BNT  */
  YYSYMBOL_BXO = 9,                        /* BXO  */
  YYSYMBOL_ADD = 10,                       /* ADD  */
  YYSYMBOL_SUB = 11,                       /* SUB  */
  YYSYMBOL_MUL = 12,                       /* MUL  */
  YYSYMBOL_DIV = 13,                       /* DIV  */
  YYSYMBOL_REM = 14,                       /* REM  */
  YYSYMBOL_NOT = 15,                       /* NOT  */
  YYSYMBOL_GTR = 16,                       /* GTR  */
  YYSYMBOL_LES = 17,                       /* LES  */
  YYSYMBOL_GEQ = 18,                       /* GEQ  */
  YYSYMBOL_LEQ = 19,                       /* LEQ  */
  YYSYMBOL_EQL = 20,                       /* EQL  */
  YYSYMBOL_NEQ = 21,                       /* NEQ  */
  YYSYMBOL_LAN = 22,                       /* LAN  */
  YYSYMBOL_LOR = 23,                       /* LOR  */
  YYSYMBOL_VAL_ASSIGN = 24,                /* VAL_ASSIGN  */
  YYSYMBOL_ADD_ASSIGN = 25,                /* ADD_ASSIGN  */
  YYSYMBOL_SUB_ASSIGN = 26,                /* SUB_ASSIGN  */
  YYSYMBOL_MUL_ASSIGN = 27,                /* MUL_ASSIGN  */
  YYSYMBOL_DIV_ASSIGN = 28,                /* DIV_ASSIGN  */
  YYSYMBOL_REM_ASSIGN = 29,                /* REM_ASSIGN  */
  YYSYMBOL_BAN_ASSIGN = 30,                /* BAN_ASSIGN  */
  YYSYMBOL_BOR_ASSIGN = 31,                /* BOR_ASSIGN  */
  YYSYMBOL_BXO_ASSIGN = 32,                /* BXO_ASSIGN  */
  YYSYMBOL_SHR_ASSIGN = 33,                /* SHR_ASSIGN  */
  YYSYMBOL_SHL_ASSIGN = 34,                /* SHL_ASSIGN  */
  YYSYMBOL_INC_ASSIGN = 35,                /* INC_ASSIGN  */
  YYSYMBOL_DEC_ASSIGN = 36,                /* DEC_ASSIGN  */
  YYSYMBOL_IF = 37,                        /* IF  */
  YYSYMBOL_ELSE = 38,                      /* ELSE  */
  YYSYMBOL_FOR = 39,                       /* FOR  */
  YYSYMBOL_WHILE = 40,                     /* WHILE  */
  YYSYMBOL_RETURN = 41,                    /* RETURN  */
  YYSYMBOL_BREAK = 42,                     /* BREAK  */
  YYSYMBOL_CONTINUE = 43,                  /* CONTINUE  */
  YYSYMBOL_VARIABLE_T = 44,                /* VARIABLE_T  */
  YYSYMBOL_BOOL_LIT = 45,                  /* BOOL_LIT  */
  YYSYMBOL_CHAR_LIT = 46,                  /* CHAR_LIT  */
  YYSYMBOL_INT_LIT = 47,                   /* INT_LIT  */
  YYSYMBOL_FLOAT_LIT = 48,                 /* FLOAT_LIT  */
  YYSYMBOL_STR_LIT = 49,                   /* STR_LIT  */
  YYSYMBOL_IDENT = 50,                     /* IDENT  */
  YYSYMBOL_THEN = 51,                      /* THEN  */
  YYSYMBOL_52_ = 52,                       /* ';'  */
  YYSYMBOL_53_ = 53,                       /* '('  */
  YYSYMBOL_54_ = 54,                       /* ')'  */
  YYSYMBOL_55_ = 55,                       /* '{'  */
  YYSYMBOL_56_ = 56,                       /* '}'  */
  YYSYMBOL_57_ = 57,                       /* ','  */
  YYSYMBOL_58_ = 58,                       /* '['  */
  YYSYMBOL_59_ = 59,                       /* ']'  */
  YYSYMBOL_60_ = 60,                       /* ':'  */
  YYSYMBOL_YYACCEPT = 61,                  /* $accept  */
  YYSYMBOL_Program = 62,                   /* Program  */
  YYSYMBOL_63_1 = 63,                      /* $@1  */
  YYSYMBOL_GlobalStmtList = 64,            /* GlobalStmtList  */
  YYSYMBOL_GlobalStmt = 65,                /* GlobalStmt  */
  YYSYMBOL_DefineVariableStmt = 66,        /* DefineVariableStmt  */
  YYSYMBOL_FunctionDefStmt = 67,           /* FunctionDefStmt  */
  YYSYMBOL_68_2 = 68,                      /* $@2  */
  YYSYMBOL_69_3 = 69,                      /* $@3  */
  YYSYMBOL_70_4 = 70,                      /* $@4  */
  YYSYMBOL_FunctionParameterStmtList = 71, /* FunctionParameterStmtList  */
  YYSYMBOL_FunctionParameterStmt = 72,     /* FunctionParameterStmt  */
  YYSYMBOL_StmtList = 73,                  /* StmtList  */
  YYSYMBOL_Stmt = 74,                      /* Stmt  */
  YYSYMBOL_75_5 = 75,                      /* $@5  */
  YYSYMBOL_76_6 = 76,                      /* $@6  */
  YYSYMBOL_77_7 = 77,                      /* $@7  */
  YYSYMBOL_78_8 = 78,                      /* $@8  */
  YYSYMBOL_79_9 = 79,                      /* $@9  */
  YYSYMBOL_80_10 = 80,                     /* $@10  */
  YYSYMBOL_Elsestmt = 81,                  /* Elsestmt  */
  YYSYMBOL_RegularDefineStmt = 82,         /* RegularDefineStmt  */
  YYSYMBOL_CoutParmListStmt = 83,          /* CoutParmListStmt  */
  YYSYMBOL_84_11 = 84,                     /* $@11  */
  YYSYMBOL_85_12 = 85,                     /* $@12  */
  YYSYMBOL_IfElseStmt = 86,                /* IfElseStmt  */
  YYSYMBOL_87_13 = 87,                     /* $@13  */
  YYSYMBOL_ForStmt = 88,                   /* ForStmt  */
  YYSYMBOL_89_14 = 89,                     /* $@14  */
  YYSYMBOL_90_15 = 90,                     /* $@15  */
  YYSYMBOL_ForInit = 91,                   /* ForInit  */
  YYSYMBOL_ArrayStmt = 92,                 /* ArrayStmt  */
  YYSYMBOL_ArrayVariable = 93,             /* ArrayVariable  */
  YYSYMBOL_ArrayExpression = 94,           /* ArrayExpression  */
  YYSYMBOL_ArrayInsideExpression = 95,     /* ArrayInsideExpression  */
  YYSYMBOL_StmtParameter = 96,             /* StmtParameter  */
  YYSYMBOL_StmtParameterList = 97,         /* StmtParameterList  */
  YYSYMBOL_Expr = 98,                      /* Expr  */
  YYSYMBOL_99_16 = 99,                     /* $@16  */
  YYSYMBOL_100_17 = 100,                   /* $@17  */
  YYSYMBOL_101_18 = 101,                   /* $@18  */
  YYSYMBOL_102_19 = 102,                   /* $@19  */
  YYSYMBOL_103_20 = 103,                   /* $@20  */
  YYSYMBOL_104_21 = 104,                   /* $@21  */
  YYSYMBOL_105_22 = 105,                   /* $@22  */
  YYSYMBOL_106_23 = 106,                   /* $@23  */
  YYSYMBOL_107_24 = 107,                   /* $@24  */
  YYSYMBOL_108_25 = 108,                   /* $@25  */
  YYSYMBOL_109_26 = 109,                   /* $@26  */
  YYSYMBOL_110_27 = 110,                   /* $@27  */
  YYSYMBOL_111_28 = 111,                   /* $@28  */
  YYSYMBOL_112_29 = 112,                   /* $@29  */
  YYSYMBOL_113_30 = 113,                   /* $@30  */
  YYSYMBOL_114_31 = 114,                   /* $@31  */
  YYSYMBOL_115_32 = 115,                   /* $@32  */
  YYSYMBOL_116_33 = 116,                   /* $@33  */
  YYSYMBOL_ExprLeftSide = 117,             /* ExprLeftSide  */
  YYSYMBOL_ExprRightSide = 118,            /* ExprRightSide  */
  YYSYMBOL_ExprDoubleAnd = 119,            /* ExprDoubleAnd  */
  YYSYMBOL_ExprOr = 120,                   /* ExprOr  */
  YYSYMBOL_ExprXor = 121,                  /* ExprXor  */
  YYSYMBOL_ExprAnd = 122,                  /* ExprAnd  */
  YYSYMBOL_ExprSame = 123,                 /* ExprSame  */
  YYSYMBOL_ExprBigOrSmall = 124,           /* ExprBigOrSmall  */
  YYSYMBOL_ExprBitwise = 125,              /* ExprBitwise  */
  YYSYMBOL_ExprPlus = 126,                 /* ExprPlus  */
  YYSYMBOL_ExprCross = 127,                /* ExprCross  */
  YYSYMBOL_ExprFrontUnary = 128,           /* ExprFrontUnary  */
  YYSYMBOL_129_34 = 129,                   /* $@34  */
  YYSYMBOL_ExprPostfix = 130,              /* ExprPostfix  */
  YYSYMBOL_ExprValue = 131                 /* ExprValue  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   415

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  61
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  71
/* YYNRULES -- Number of rules.  */
#define YYNRULES  147
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  244

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   306


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      53,    54,     2,     2,    57,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    60,    52,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    58,     2,    59,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    55,     2,    56,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   121,   121,   121,   122,   126,   127,   131,   132,   136,
     141,   141,   141,   141,   145,   146,   147,   151,   152,   153,
     158,   159,   163,   164,   165,   166,   166,   166,   167,   167,
     167,   167,   168,   168,   169,   170,   171,   172,   176,   177,
     181,   182,   183,   184,   188,   188,   189,   189,   193,   194,
     194,   198,   198,   198,   199,   203,   204,   208,   209,   213,
     214,   218,   222,   223,   224,   228,   229,   233,   234,   238,
     239,   239,   239,   240,   240,   240,   241,   241,   241,   242,
     242,   242,   243,   243,   243,   244,   244,   244,   245,   245,
     245,   246,   246,   246,   247,   247,   247,   248,   252,   253,
     254,   258,   259,   263,   264,   268,   269,   273,   274,   278,
     279,   283,   284,   285,   289,   290,   291,   292,   293,   297,
     298,   299,   303,   304,   305,   309,   310,   311,   312,   316,
     317,   318,   319,   320,   320,   321,   325,   326,   327,   331,
     332,   333,   334,   335,   336,   337,   338,   339
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "COUT", "SHR", "SHL",
  "BAN", "BOR", "BNT", "BXO", "ADD", "SUB", "MUL", "DIV", "REM", "NOT",
  "GTR", "LES", "GEQ", "LEQ", "EQL", "NEQ", "LAN", "LOR", "VAL_ASSIGN",
  "ADD_ASSIGN", "SUB_ASSIGN", "MUL_ASSIGN", "DIV_ASSIGN", "REM_ASSIGN",
  "BAN_ASSIGN", "BOR_ASSIGN", "BXO_ASSIGN", "SHR_ASSIGN", "SHL_ASSIGN",
  "INC_ASSIGN", "DEC_ASSIGN", "IF", "ELSE", "FOR", "WHILE", "RETURN",
  "BREAK", "CONTINUE", "VARIABLE_T", "BOOL_LIT", "CHAR_LIT", "INT_LIT",
  "FLOAT_LIT", "STR_LIT", "IDENT", "THEN", "';'", "'('", "')'", "'{'",
  "'}'", "','", "'['", "']'", "':'", "$accept", "Program", "$@1",
  "GlobalStmtList", "GlobalStmt", "DefineVariableStmt", "FunctionDefStmt",
  "$@2", "$@3", "$@4", "FunctionParameterStmtList",
  "FunctionParameterStmt", "StmtList", "Stmt", "$@5", "$@6", "$@7", "$@8",
  "$@9", "$@10", "Elsestmt", "RegularDefineStmt", "CoutParmListStmt",
  "$@11", "$@12", "IfElseStmt", "$@13", "ForStmt", "$@14", "$@15",
  "ForInit", "ArrayStmt", "ArrayVariable", "ArrayExpression",
  "ArrayInsideExpression", "StmtParameter", "StmtParameterList", "Expr",
  "$@16", "$@17", "$@18", "$@19", "$@20", "$@21", "$@22", "$@23", "$@24",
  "$@25", "$@26", "$@27", "$@28", "$@29", "$@30", "$@31", "$@32", "$@33",
  "ExprLeftSide", "ExprRightSide", "ExprDoubleAnd", "ExprOr", "ExprXor",
  "ExprAnd", "ExprSame", "ExprBigOrSmall", "ExprBitwise", "ExprPlus",
  "ExprCross", "ExprFrontUnary", "$@34", "ExprPostfix", "ExprValue", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-199)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-101)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
       9,    28,    -2,  -199,    -3,    -2,  -199,  -199,  -199,    25,
    -199,   353,    14,   353,   353,   353,   353,  -199,  -199,  -199,
    -199,  -199,   -12,   132,     2,    43,    69,    82,   107,    37,
      55,    93,    84,   139,  -199,  -199,   119,  -199,  -199,  -199,
    -199,  -199,   289,   -21,    64,    79,   -14,   132,    87,   352,
     113,   353,  -199,   353,   353,   353,   353,   353,   353,   353,
     353,   353,   353,   353,   353,   353,   353,   353,   353,   353,
    -199,  -199,   289,    88,    -1,  -199,    84,  -199,  -199,    85,
     -21,  -199,    90,    92,  -199,   362,   124,   131,   138,   143,
     137,   145,   142,   150,   153,    43,    69,    82,   107,    37,
      55,    55,    93,    93,    93,    93,    84,    84,   139,   139,
    -199,  -199,  -199,    12,   130,  -199,   289,  -199,   133,   -13,
     101,  -199,   362,   362,   362,   362,   362,   362,   362,   362,
     362,  -199,   134,  -199,  -199,   362,  -199,  -199,  -199,  -199,
    -199,  -199,  -199,  -199,  -199,  -199,   135,  -199,  -199,  -199,
    -199,  -199,  -199,  -199,  -199,  -199,  -199,   279,   186,   141,
    -199,  -199,   298,   158,   162,   166,  -199,    40,  -199,   167,
    -199,  -199,   170,   362,   171,   173,  -199,   175,  -199,  -199,
      -6,   172,  -199,  -199,  -199,  -199,   353,  -199,   190,   169,
     344,   362,  -199,   353,     6,   178,    84,   353,  -199,   174,
     176,  -199,  -199,   113,   177,   209,  -199,    84,   279,   362,
     180,   185,   187,   353,  -199,  -199,  -199,   189,  -199,   279,
     362,  -199,   102,    84,   202,   279,   159,   193,   191,  -199,
     353,   279,  -199,   210,  -199,  -199,   279,    84,  -199,  -199,
     362,   228,  -199,  -199
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     0,     1,     0,     3,     6,     7,     8,    10,
       5,     0,     0,     0,     0,     0,     0,   141,   142,   139,
     140,   143,   145,     0,     0,   102,   104,   106,   108,   110,
     113,   118,   121,   124,   128,   135,   138,    11,   130,   131,
     132,   129,    16,     0,   146,     0,   145,     0,     0,    70,
      97,     0,     9,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     136,   137,    16,     0,     0,    15,    19,    60,    59,     0,
       0,   133,   146,     0,   147,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   101,   103,   105,   107,   109,
     111,   112,   115,   117,   114,   116,   119,   120,   122,   123,
     125,   126,   127,     0,    17,   144,     0,    58,     0,     0,
     147,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    12,     0,    14,    57,     0,   134,    71,    74,    77,
      80,    83,    86,    89,    92,    95,     0,    18,    72,    75,
      78,    81,    84,    87,    90,    93,    96,     0,     0,     0,
      32,    28,     0,     0,     0,     0,    22,     0,    21,     0,
      43,    46,    44,     0,     0,     0,    36,     0,    34,    35,
      68,    40,    66,    13,    20,    23,     0,    24,     0,     0,
      56,     0,    37,     0,    41,     0,    47,     0,    25,    55,
       0,    51,    29,    67,     0,    68,    65,    45,    49,     0,
       0,     0,     0,    64,    42,    48,    26,     0,    54,     0,
       0,    30,     0,    63,    39,     0,     0,     0,     0,    61,
       0,    49,    27,     0,    33,    52,     0,    62,    38,    50,
       0,     0,    53,    31
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -199,  -199,  -199,  -199,   237,  -199,  -199,  -199,  -199,  -199,
     192,   163,  -198,  -163,  -199,  -199,  -199,  -199,  -199,  -199,
    -199,    58,  -199,  -199,  -199,    30,  -199,  -199,  -199,  -199,
    -199,   -43,   203,  -199,  -199,  -199,    91,   -23,  -199,  -199,
    -199,  -199,  -199,  -199,  -199,  -199,  -199,  -199,  -199,  -199,
    -199,  -199,  -199,  -199,  -199,  -199,  -199,    -9,   234,   235,
     238,   236,   239,   103,    49,   -41,    98,    -8,  -199,   179,
    -199
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     1,     2,     5,     6,     7,     8,    12,    72,   146,
      74,    75,   167,   168,   208,   224,   175,   212,   228,   174,
     232,   169,   172,   188,   186,   216,   217,   200,   211,   240,
     201,    44,    79,   214,   222,   181,   182,   170,    86,   148,
      87,   149,    88,   150,    89,   151,    90,   152,    91,   153,
      92,   154,    93,   155,    94,   156,    49,    50,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,   119,    35,
      36
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      48,    76,    24,    82,   184,    38,    39,    40,    41,    -4,
     -98,   -98,   -98,   -98,   -98,   -98,   -98,   -98,   193,   -98,
     -98,   226,   106,   107,    83,    51,    77,   233,     3,    78,
     204,    76,    17,    18,    19,    20,    21,    22,   241,    42,
     135,    42,     4,   158,    43,   215,    43,     9,    13,    11,
      14,    15,    43,   115,    52,    16,   116,    57,    58,   110,
     111,   112,   121,   184,    80,    53,   131,    37,   215,   116,
     184,    59,    60,    61,    62,    76,    54,   159,   184,   160,
     161,   162,   163,   164,   165,    17,    18,    19,    20,    21,
      46,    55,   166,    47,    65,    66,   183,    63,    64,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   102,   103,
     104,   105,    48,    56,   -99,   -99,   -99,   -99,   -99,   -99,
     -99,   -99,    80,   -99,   -99,  -100,  -100,  -100,  -100,  -100,
    -100,  -100,  -100,    81,  -100,  -100,    51,   194,   114,   177,
      13,    84,    14,    15,   117,   196,   120,    16,    80,   122,
     189,    67,    68,    69,    70,    71,   207,   123,   229,   230,
     100,   101,   158,   108,   109,   124,   126,    13,   202,    14,
      15,   125,   223,   128,    16,   127,    45,    17,    18,    19,
      20,    21,    46,   129,   203,    47,   218,   130,   132,   237,
     157,   171,   134,   147,   173,   197,   159,   227,   160,   161,
     162,   163,   164,   165,    17,    18,    19,    20,    21,    46,
     178,   166,    47,   158,   179,   234,   180,   242,    13,   185,
      14,    15,   187,   198,   190,    16,   191,   192,   205,   195,
     210,   158,   213,   193,   209,   219,    13,   220,    14,    15,
     231,   221,    10,    16,   225,   235,   236,   159,   199,   160,
     161,   162,   163,   164,   165,    17,    18,    19,    20,    21,
      46,   238,   166,    47,   113,   159,   239,   160,   161,   162,
     163,   164,   165,    17,    18,    19,    20,    21,    46,   133,
     166,    47,   158,   118,   243,    95,   206,    13,    96,    14,
      15,    98,    97,     0,    16,    99,     0,    13,   136,    14,
      15,     0,     0,     0,    16,     0,    13,     0,    14,    15,
       0,     0,     0,    16,     0,     0,   159,     0,   160,   161,
     162,   163,   164,   165,    17,    18,    19,    20,    21,    46,
       0,   166,    47,    73,    17,    18,    19,    20,    21,    22,
       0,     0,    23,    17,    18,    19,    20,    21,    46,     0,
     176,    47,    13,     0,    14,    15,     0,     0,     0,    16,
       0,    13,     0,    14,    15,     0,     0,     0,    16,     0,
      13,     0,    14,    15,     0,     0,    85,    16,   -73,   -76,
     -79,   -82,   -85,   -88,     0,   -91,   -94,     0,   165,    17,
      18,    19,    20,    21,    46,     0,     0,    47,    17,    18,
      19,    20,    21,    22,     0,     0,    23,    17,    18,    19,
      20,    21,    46,     0,     0,    47
};

static const yytype_int16 yycheck[] =
{
      23,    42,    11,    46,   167,    13,    14,    15,    16,     0,
      24,    25,    26,    27,    28,    29,    30,    31,    24,    33,
      34,   219,    63,    64,    47,    23,    47,   225,     0,    50,
      24,    72,    45,    46,    47,    48,    49,    50,   236,    53,
      53,    53,    44,     3,    58,   208,    58,    50,     8,    24,
      10,    11,    58,    54,    52,    15,    57,    20,    21,    67,
      68,    69,    85,   226,    58,    22,    54,    53,   231,    57,
     233,    16,    17,    18,    19,   116,     7,    37,   241,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,     9,    52,    53,    10,    11,    56,     4,     5,   122,
     123,   124,   125,   126,   127,   128,   129,   130,    59,    60,
      61,    62,   135,     6,    24,    25,    26,    27,    28,    29,
      30,    31,    58,    33,    34,    24,    25,    26,    27,    28,
      29,    30,    31,    54,    33,    34,    23,   180,    50,   162,
       8,    54,    10,    11,    59,   186,    54,    15,    58,    25,
     173,    12,    13,    14,    35,    36,   197,    26,    56,    57,
      57,    58,     3,    65,    66,    27,    29,     8,   191,    10,
      11,    28,   213,    31,    15,    30,    44,    45,    46,    47,
      48,    49,    50,    33,   193,    53,   209,    34,    58,   230,
      55,     5,    59,    59,    53,     5,    37,   220,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      52,    52,    53,     3,    52,    56,    50,   240,     8,    52,
      10,    11,    52,    54,    53,    15,    53,    52,    50,    57,
      54,     3,    55,    24,    60,    55,     8,    52,    10,    11,
      38,    54,     5,    15,    55,    52,    55,    37,   190,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,   231,    52,    53,    72,    37,    56,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,   116,
      52,    53,     3,    80,    56,    51,   195,     8,    53,    10,
      11,    55,    54,    -1,    15,    56,    -1,     8,   119,    10,
      11,    -1,    -1,    -1,    15,    -1,     8,    -1,    10,    11,
      -1,    -1,    -1,    15,    -1,    -1,    37,    -1,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      -1,    52,    53,    44,    45,    46,    47,    48,    49,    50,
      -1,    -1,    53,    45,    46,    47,    48,    49,    50,    -1,
      52,    53,     8,    -1,    10,    11,    -1,    -1,    -1,    15,
      -1,     8,    -1,    10,    11,    -1,    -1,    -1,    15,    -1,
       8,    -1,    10,    11,    -1,    -1,    24,    15,    26,    27,
      28,    29,    30,    31,    -1,    33,    34,    -1,    44,    45,
      46,    47,    48,    49,    50,    -1,    -1,    53,    45,    46,
      47,    48,    49,    50,    -1,    -1,    53,    45,    46,    47,
      48,    49,    50,    -1,    -1,    53
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    62,    63,     0,    44,    64,    65,    66,    67,    50,
      65,    24,    68,     8,    10,    11,    15,    45,    46,    47,
      48,    49,    50,    53,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   130,   131,    53,   128,   128,
     128,   128,    53,    58,    92,    44,    50,    53,    98,   117,
     118,    23,    52,    22,     7,     9,     6,    20,    21,    16,
      17,    18,    19,     4,     5,    10,    11,    12,    13,    14,
      35,    36,    69,    44,    71,    72,   126,    47,    50,    93,
      58,    54,    92,    98,    54,    24,    99,   101,   103,   105,
     107,   109,   111,   113,   115,   119,   120,   121,   122,   123,
     124,   124,   125,   125,   125,   125,   126,   126,   127,   127,
     128,   128,   128,    71,    50,    54,    57,    59,    93,   129,
      54,    98,    25,    26,    27,    28,    29,    30,    31,    33,
      34,    54,    58,    72,    59,    53,   130,    98,    98,    98,
      98,    98,    98,    98,    98,    98,    70,    59,   100,   102,
     104,   106,   108,   110,   112,   114,   116,    55,     3,    37,
      39,    40,    41,    42,    43,    44,    52,    73,    74,    82,
      98,     5,    83,    53,    80,    77,    52,    98,    52,    52,
      50,    96,    97,    56,    74,    52,    85,    52,    84,    98,
      53,    53,    52,    24,    92,    57,   126,     5,    54,    82,
      88,    91,    98,   118,    24,    50,    97,   126,    75,    60,
      54,    89,    78,    55,    94,    74,    86,    87,    98,    55,
      52,    54,    95,   126,    76,    55,    73,    98,    79,    56,
      57,    38,    81,    73,    56,    52,    55,   126,    86,    56,
      90,    73,    98,    56
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,    61,    63,    62,    62,    64,    64,    65,    65,    66,
      68,    69,    70,    67,    71,    71,    71,    72,    72,    72,
      73,    73,    74,    74,    74,    75,    76,    74,    77,    78,
      79,    74,    80,    74,    74,    74,    74,    74,    81,    81,
      82,    82,    82,    82,    84,    83,    85,    83,    86,    87,
      86,    89,    90,    88,    88,    91,    91,    92,    92,    93,
      93,    94,    95,    95,    95,    96,    96,    97,    97,    98,
      99,   100,    98,   101,   102,    98,   103,   104,    98,   105,
     106,    98,   107,   108,    98,   109,   110,    98,   111,   112,
      98,   113,   114,    98,   115,   116,    98,    98,   117,   117,
     117,   118,   118,   119,   119,   120,   120,   121,   121,   122,
     122,   123,   123,   123,   124,   124,   124,   124,   124,   125,
     125,   125,   126,   126,   126,   127,   127,   127,   127,   128,
     128,   128,   128,   129,   128,   128,   130,   130,   130,   131,
     131,   131,   131,   131,   131,   131,   131,   131
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     0,     2,     1,     1,     1,     5,
       0,     0,     0,    11,     3,     1,     0,     2,     4,     1,
       2,     1,     1,     2,     3,     0,     0,     8,     0,     0,
       0,    10,     0,     8,     2,     2,     2,     3,     2,     0,
       2,     3,     5,     1,     0,     4,     0,     3,     1,     0,
       4,     0,     0,     7,     3,     1,     0,     4,     3,     1,
       1,     3,     3,     1,     0,     3,     1,     3,     1,     3,
       0,     0,     5,     0,     0,     5,     0,     0,     5,     0,
       0,     5,     0,     0,     5,     0,     0,     5,     0,     0,
       5,     0,     0,     5,     0,     0,     5,     1,     1,     2,
       3,     3,     1,     3,     1,     3,     1,     3,     1,     3,
       1,     3,     3,     1,     3,     3,     3,     3,     1,     3,
       3,     1,     3,     3,     1,     3,     3,     3,     1,     2,
       2,     2,     2,     0,     5,     1,     2,     2,     1,     1,
       1,     1,     1,     1,     4,     1,     2,     3
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* $@1: %empty  */
#line 121 "compiler.y"
      { init_jfile(); syT=crt_SymbolT(++scope); }
#line 1585 "compiler.tab.c"
    break;

  case 3: /* Program: $@1 GlobalStmtList  */
#line 121 "compiler.y"
                                                                 { scope--; close_jfile(); }
#line 1591 "compiler.tab.c"
    break;

  case 10: /* $@2: %empty  */
#line 141 "compiler.y"
                       { ins_SymbolT(0,(yyvsp[0].s_var),-1,yylineno,"unknown",scope); }
#line 1597 "compiler.tab.c"
    break;

  case 11: /* $@3: %empty  */
#line 141 "compiler.y"
                                                                                     { syT=crt_SymbolT(++scope); }
#line 1603 "compiler.tab.c"
    break;

  case 12: /* $@4: %empty  */
#line 141 "compiler.y"
                                                                                                                                                 { func_sig_array[func_sig_counter++]=(yyvsp[-6].var_type); construct_func((yyvsp[-5].var_type)); ins_func_sig((yyvsp[-5].s_var)); writeFunc_jfile((yyvsp[-5].s_var),find_call((yyvsp[-5].s_var))); }
#line 1609 "compiler.tab.c"
    break;

  case 13: /* FunctionDefStmt: VARIABLE_T IDENT $@2 '(' $@3 FunctionParameterStmtList ')' $@4 '{' StmtList '}'  */
#line 141 "compiler.y"
                                                                                                                                                                                                                                                                                                                               {scope--;}
#line 1615 "compiler.tab.c"
    break;

  case 17: /* FunctionParameterStmt: VARIABLE_T IDENT  */
#line 151 "compiler.y"
                       { ins_SymbolT((yyvsp[-1].var_type),(yyvsp[0].s_var),address++,yylineno,"-",scope);  func_sig_array[func_sig_counter++]=(yyvsp[-1].var_type); }
#line 1621 "compiler.tab.c"
    break;

  case 18: /* FunctionParameterStmt: VARIABLE_T IDENT '[' ']'  */
#line 152 "compiler.y"
                               { ins_SymbolT((yyvsp[-3].var_type),(yyvsp[-2].s_var),address++,yylineno,"-",scope); func_sig_array[func_sig_counter++]=((yyvsp[-3].var_type)+12); }
#line 1627 "compiler.tab.c"
    break;

  case 25: /* $@5: %empty  */
#line 166 "compiler.y"
                      { ifElseStack[++ifElseStack_counter]=compare_count; compare_count++; fprintf(jfile, "\tifeq If%d_outside1\n",ifElseStack[ifElseStack_counter]); }
#line 1633 "compiler.tab.c"
    break;

  case 26: /* $@6: %empty  */
#line 166 "compiler.y"
                                                                                                                                                                                   { fprintf(jfile, "\tgoto If%d_outside2\n",ifElseStack[ifElseStack_counter]); fprintf(jfile, "If%d_outside1:\n",ifElseStack[ifElseStack_counter]); }
#line 1639 "compiler.tab.c"
    break;

  case 27: /* Stmt: IF '(' Expr ')' $@5 IfElseStmt $@6 Elsestmt  */
#line 166 "compiler.y"
                                                                                                                                                                                                                                                                                                                                                { fprintf(jfile, "If%d_outside2:\n",ifElseStack[ifElseStack_counter--]); }
#line 1645 "compiler.tab.c"
    break;

  case 28: /* $@7: %empty  */
#line 167 "compiler.y"
            { fprintf(jfile, "While%d_inside1:\n",++while_count); }
#line 1651 "compiler.tab.c"
    break;

  case 29: /* $@8: %empty  */
#line 167 "compiler.y"
                                                                             { syT=crt_SymbolT(++scope); }
#line 1657 "compiler.tab.c"
    break;

  case 30: /* $@9: %empty  */
#line 167 "compiler.y"
                                                                                                               { fprintf(jfile, "\tifeq While%d_outside1\n",while_count); }
#line 1663 "compiler.tab.c"
    break;

  case 31: /* Stmt: WHILE $@7 '(' Expr $@8 ')' $@9 '{' StmtList '}'  */
#line 167 "compiler.y"
                                                                                                                                                                                             { fprintf(jfile, "goto While%d_inside1\n",while_count); fprintf(jfile, "While%d_outside1:\n",while_count); }
#line 1669 "compiler.tab.c"
    break;

  case 32: /* $@10: %empty  */
#line 168 "compiler.y"
          { syT=crt_SymbolT(++scope); }
#line 1675 "compiler.tab.c"
    break;

  case 33: /* Stmt: FOR $@10 '(' ForStmt ')' '{' StmtList '}'  */
#line 168 "compiler.y"
                                                                         { fprintf(jfile, "goto For%d_inside\n",for_count); fprintf(jfile, "For%d_outside:\n",for_count); }
#line 1681 "compiler.tab.c"
    break;

  case 40: /* RegularDefineStmt: VARIABLE_T StmtParameter  */
#line 181 "compiler.y"
                               { if((yyvsp[-1].var_type)!=1){ cur_value_type=(yyvsp[-1].var_type); } stack_type[type_no++]=cur_value_type; }
#line 1687 "compiler.tab.c"
    break;

  case 41: /* RegularDefineStmt: VARIABLE_T IDENT ArrayStmt  */
#line 182 "compiler.y"
                                 { ins_SymbolT((yyvsp[-2].var_type),(yyvsp[-1].s_var),address++,yylineno,"-",scope); }
#line 1693 "compiler.tab.c"
    break;

  case 42: /* RegularDefineStmt: VARIABLE_T IDENT ArrayStmt VAL_ASSIGN ArrayExpression  */
#line 183 "compiler.y"
                                                            { ins_SymbolT((yyvsp[-4].var_type),(yyvsp[-3].s_var),address++,yylineno,"-",scope); }
#line 1699 "compiler.tab.c"
    break;

  case 44: /* $@11: %empty  */
#line 188 "compiler.y"
                       {fprintf(jfile, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");}
#line 1705 "compiler.tab.c"
    break;

  case 45: /* CoutParmListStmt: CoutParmListStmt $@11 SHL ExprPlus  */
#line 188 "compiler.y"
                                                                                                                   {print_cout();}
#line 1711 "compiler.tab.c"
    break;

  case 46: /* $@12: %empty  */
#line 189 "compiler.y"
          {fprintf(jfile, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");}
#line 1717 "compiler.tab.c"
    break;

  case 47: /* CoutParmListStmt: SHL $@12 ExprPlus  */
#line 189 "compiler.y"
                                                                                                 {print_cout();}
#line 1723 "compiler.tab.c"
    break;

  case 49: /* $@13: %empty  */
#line 194 "compiler.y"
      { syT=crt_SymbolT(++scope); }
#line 1729 "compiler.tab.c"
    break;

  case 50: /* IfElseStmt: $@13 '{' StmtList '}'  */
#line 194 "compiler.y"
                                                     { scope--; }
#line 1735 "compiler.tab.c"
    break;

  case 51: /* $@14: %empty  */
#line 198 "compiler.y"
              { fprintf(jfile, "For%d_inside:\n",++for_count); }
#line 1741 "compiler.tab.c"
    break;

  case 52: /* $@15: %empty  */
#line 198 "compiler.y"
                                                                              { fprintf(jfile, "ifeq For%d_outside\n",for_count); }
#line 1747 "compiler.tab.c"
    break;

  case 67: /* StmtParameterList: IDENT VAL_ASSIGN ExprRightSide  */
#line 233 "compiler.y"
                                     {iORf(); fprintf(jfile, "store %d\n",address); ins_SymbolT(type_no,(yyvsp[-2].s_var),address++,yylineno,"-",scope); }
#line 1753 "compiler.tab.c"
    break;

  case 68: /* StmtParameterList: IDENT  */
#line 234 "compiler.y"
            { ins_SymbolT(type_no,(yyvsp[0].s_var),address++,yylineno,"-",scope); }
#line 1759 "compiler.tab.c"
    break;

  case 69: /* Expr: ExprLeftSide VAL_ASSIGN Expr  */
#line 238 "compiler.y"
                                   { if(cur_value_type!=findTypeWithAddr(store_addr)) { casting(findTypeWithAddr(store_addr)); } iORf(); fprintf(jfile, "store %d\n",store_addr); }
#line 1765 "compiler.tab.c"
    break;

  case 70: /* $@16: %empty  */
#line 239 "compiler.y"
                   { iORf(); fprintf(jfile, "load %d\n",store_addr); }
#line 1771 "compiler.tab.c"
    break;

  case 71: /* $@17: %empty  */
#line 239 "compiler.y"
                                                                                       { iORf(); fprintf(jfile, "add\n"); }
#line 1777 "compiler.tab.c"
    break;

  case 72: /* Expr: ExprLeftSide $@16 ADD_ASSIGN Expr $@17  */
#line 239 "compiler.y"
                                                                                                                            { iORf(); fprintf(jfile, "store %d\n",store_addr); }
#line 1783 "compiler.tab.c"
    break;

  case 73: /* $@18: %empty  */
#line 240 "compiler.y"
                   { iORf(); fprintf(jfile, "load %d\n",store_addr); }
#line 1789 "compiler.tab.c"
    break;

  case 74: /* $@19: %empty  */
#line 240 "compiler.y"
                                                                                       { iORf(); fprintf(jfile, "sub\n"); }
#line 1795 "compiler.tab.c"
    break;

  case 75: /* Expr: ExprLeftSide $@18 SUB_ASSIGN Expr $@19  */
#line 240 "compiler.y"
                                                                                                                            { iORf(); fprintf(jfile, "store %d\n",store_addr); }
#line 1801 "compiler.tab.c"
    break;

  case 76: /* $@20: %empty  */
#line 241 "compiler.y"
                   { iORf(); fprintf(jfile, "load %d\n",store_addr); }
#line 1807 "compiler.tab.c"
    break;

  case 77: /* $@21: %empty  */
#line 241 "compiler.y"
                                                                                       { iORf(); fprintf(jfile, "mul\n"); }
#line 1813 "compiler.tab.c"
    break;

  case 78: /* Expr: ExprLeftSide $@20 MUL_ASSIGN Expr $@21  */
#line 241 "compiler.y"
                                                                                                                            { iORf(); fprintf(jfile, "store %d\n",store_addr); }
#line 1819 "compiler.tab.c"
    break;

  case 79: /* $@22: %empty  */
#line 242 "compiler.y"
                   { iORf(); fprintf(jfile, "load %d\n",store_addr); }
#line 1825 "compiler.tab.c"
    break;

  case 80: /* $@23: %empty  */
#line 242 "compiler.y"
                                                                                       { iORf(); fprintf(jfile, "div\n"); }
#line 1831 "compiler.tab.c"
    break;

  case 81: /* Expr: ExprLeftSide $@22 DIV_ASSIGN Expr $@23  */
#line 242 "compiler.y"
                                                                                                                            { iORf(); fprintf(jfile, "store %d\n",store_addr); }
#line 1837 "compiler.tab.c"
    break;

  case 82: /* $@24: %empty  */
#line 243 "compiler.y"
                   { iORf(); fprintf(jfile, "load %d\n",store_addr); }
#line 1843 "compiler.tab.c"
    break;

  case 83: /* $@25: %empty  */
#line 243 "compiler.y"
                                                                                       { iORf(); fprintf(jfile, "rem\n"); }
#line 1849 "compiler.tab.c"
    break;

  case 84: /* Expr: ExprLeftSide $@24 REM_ASSIGN Expr $@25  */
#line 243 "compiler.y"
                                                                                                                            { iORf(); fprintf(jfile, "store %d\n",store_addr); }
#line 1855 "compiler.tab.c"
    break;

  case 85: /* $@26: %empty  */
#line 244 "compiler.y"
                   { iORf(); fprintf(jfile, "load %d\n",store_addr); }
#line 1861 "compiler.tab.c"
    break;

  case 86: /* $@27: %empty  */
#line 244 "compiler.y"
                                                                                       { iORf(); fprintf(jfile, "and\n"); }
#line 1867 "compiler.tab.c"
    break;

  case 87: /* Expr: ExprLeftSide $@26 BAN_ASSIGN Expr $@27  */
#line 244 "compiler.y"
                                                                                                                            { iORf(); fprintf(jfile, "store %d\n",store_addr); }
#line 1873 "compiler.tab.c"
    break;

  case 88: /* $@28: %empty  */
#line 245 "compiler.y"
                   { iORf(); fprintf(jfile, "load %d\n",store_addr); }
#line 1879 "compiler.tab.c"
    break;

  case 89: /* $@29: %empty  */
#line 245 "compiler.y"
                                                                                       { iORf(); fprintf(jfile, "or\n"); }
#line 1885 "compiler.tab.c"
    break;

  case 90: /* Expr: ExprLeftSide $@28 BOR_ASSIGN Expr $@29  */
#line 245 "compiler.y"
                                                                                                                           { iORf(); fprintf(jfile, "store %d\n",store_addr); }
#line 1891 "compiler.tab.c"
    break;

  case 91: /* $@30: %empty  */
#line 246 "compiler.y"
                   { iORf(); fprintf(jfile, "load %d\n",store_addr); }
#line 1897 "compiler.tab.c"
    break;

  case 92: /* $@31: %empty  */
#line 246 "compiler.y"
                                                                                       { iORf(); fprintf(jfile, "shr\n"); }
#line 1903 "compiler.tab.c"
    break;

  case 93: /* Expr: ExprLeftSide $@30 SHR_ASSIGN Expr $@31  */
#line 246 "compiler.y"
                                                                                                                            { iORf(); fprintf(jfile, "store %d\n",store_addr); }
#line 1909 "compiler.tab.c"
    break;

  case 94: /* $@32: %empty  */
#line 247 "compiler.y"
                   { iORf(); fprintf(jfile, "load %d\n",store_addr); }
#line 1915 "compiler.tab.c"
    break;

  case 95: /* $@33: %empty  */
#line 247 "compiler.y"
                                                                                       { iORf(); fprintf(jfile, "shl\n"); }
#line 1921 "compiler.tab.c"
    break;

  case 96: /* Expr: ExprLeftSide $@32 SHL_ASSIGN Expr $@33  */
#line 247 "compiler.y"
                                                                                                                            { iORf(); fprintf(jfile, "store %d\n",store_addr); }
#line 1927 "compiler.tab.c"
    break;

  case 98: /* ExprLeftSide: IDENT  */
#line 252 "compiler.y"
            { store_addr=find_ident_address((yyvsp[0].s_var)); cur_value_type=find_ident_type((yyvsp[0].s_var)); }
#line 1933 "compiler.tab.c"
    break;

  case 101: /* ExprRightSide: ExprRightSide LOR ExprDoubleAnd  */
#line 258 "compiler.y"
                                      { fprintf(jfile, "\tior ;\n" );}
#line 1939 "compiler.tab.c"
    break;

  case 103: /* ExprDoubleAnd: ExprDoubleAnd LAN ExprOr  */
#line 263 "compiler.y"
                               { fprintf(jfile, "\tiand ;\n" ); }
#line 1945 "compiler.tab.c"
    break;

  case 105: /* ExprOr: ExprOr BOR ExprXor  */
#line 268 "compiler.y"
                         { fprintf(jfile, "\tior ;\n" ); }
#line 1951 "compiler.tab.c"
    break;

  case 107: /* ExprXor: ExprXor BXO ExprAnd  */
#line 273 "compiler.y"
                          { fprintf(jfile, "\tixor\n" ); }
#line 1957 "compiler.tab.c"
    break;

  case 109: /* ExprAnd: ExprAnd BAN ExprSame  */
#line 278 "compiler.y"
                           { fprintf(jfile, "\tiand\n" ); }
#line 1963 "compiler.tab.c"
    break;

  case 111: /* ExprSame: ExprSame EQL ExprBigOrSmall  */
#line 283 "compiler.y"
                                  { opEQL(); }
#line 1969 "compiler.tab.c"
    break;

  case 112: /* ExprSame: ExprSame NEQ ExprBigOrSmall  */
#line 284 "compiler.y"
                                  { opNEQ(); }
#line 1975 "compiler.tab.c"
    break;

  case 114: /* ExprBigOrSmall: ExprBigOrSmall GEQ ExprBitwise  */
#line 289 "compiler.y"
                                     { opGEQ(); }
#line 1981 "compiler.tab.c"
    break;

  case 115: /* ExprBigOrSmall: ExprBigOrSmall GTR ExprBitwise  */
#line 290 "compiler.y"
                                     { opGTR(); }
#line 1987 "compiler.tab.c"
    break;

  case 117: /* ExprBigOrSmall: ExprBigOrSmall LES ExprBitwise  */
#line 292 "compiler.y"
                                     { opLES(); }
#line 1993 "compiler.tab.c"
    break;

  case 119: /* ExprBitwise: ExprBitwise SHR ExprPlus  */
#line 297 "compiler.y"
                               { fprintf(jfile, "\tishr ;\n" ); }
#line 1999 "compiler.tab.c"
    break;

  case 120: /* ExprBitwise: ExprBitwise SHL ExprPlus  */
#line 298 "compiler.y"
                               { fprintf(jfile, "\tishl ;\n" ); }
#line 2005 "compiler.tab.c"
    break;

  case 122: /* ExprPlus: ExprPlus ADD ExprCross  */
#line 303 "compiler.y"
                            { if(cur_value_type==7) fprintf(jfile, "\tiadd ;\n" ); if(cur_value_type==9) fprintf(jfile, "\tfadd ;\n" );}
#line 2011 "compiler.tab.c"
    break;

  case 123: /* ExprPlus: ExprPlus SUB ExprCross  */
#line 304 "compiler.y"
                            { if(cur_value_type==7) fprintf(jfile, "\tisub ;\n" ); if(cur_value_type==9) fprintf(jfile, "\tfsub ;\n" );}
#line 2017 "compiler.tab.c"
    break;

  case 125: /* ExprCross: ExprCross MUL ExprFrontUnary  */
#line 309 "compiler.y"
                                   { if(cur_value_type==7) fprintf(jfile, "\timul ;\n" ); if(cur_value_type==9) fprintf(jfile, "\tfmul ;\n" );}
#line 2023 "compiler.tab.c"
    break;

  case 126: /* ExprCross: ExprCross DIV ExprFrontUnary  */
#line 310 "compiler.y"
                                   { if(cur_value_type==7) fprintf(jfile, "\tidiv ;\n" ); if(cur_value_type==9) fprintf(jfile, "\tfdiv ;\n" );}
#line 2029 "compiler.tab.c"
    break;

  case 127: /* ExprCross: ExprCross REM ExprFrontUnary  */
#line 311 "compiler.y"
                                   { fprintf(jfile, "\tirem ;\n" );}
#line 2035 "compiler.tab.c"
    break;

  case 129: /* ExprFrontUnary: NOT ExprFrontUnary  */
#line 316 "compiler.y"
                         { opNOT(); }
#line 2041 "compiler.tab.c"
    break;

  case 130: /* ExprFrontUnary: BNT ExprFrontUnary  */
#line 317 "compiler.y"
                         { opBNT(); }
#line 2047 "compiler.tab.c"
    break;

  case 132: /* ExprFrontUnary: SUB ExprFrontUnary  */
#line 319 "compiler.y"
                         { if(cur_value_type==7) fprintf(jfile, "\tineg ;\n" ); if(cur_value_type==9) fprintf(jfile, "\tfneg ;\n" );}
#line 2053 "compiler.tab.c"
    break;

  case 133: /* $@34: %empty  */
#line 320 "compiler.y"
                         { cur_addr=-1; }
#line 2059 "compiler.tab.c"
    break;

  case 134: /* ExprFrontUnary: '(' VARIABLE_T ')' $@34 ExprPostfix  */
#line 320 "compiler.y"
                                                      {cur_value_type=(yyvsp[-3].var_type); casting(cur_value_type);  }
#line 2065 "compiler.tab.c"
    break;

  case 136: /* ExprPostfix: ExprValue INC_ASSIGN  */
#line 325 "compiler.y"
                           { fprintf(jfile, "\tldc 1\n"); fprintf(jfile, "\tiadd\n"); fprintf(jfile, "\tistore %d\n",cur_addr); }
#line 2071 "compiler.tab.c"
    break;

  case 139: /* ExprValue: INT_LIT  */
#line 331 "compiler.y"
              { lineType=cur_value_type=7; fprintf(jfile, "\tldc %d ;\n",(yyvsp[0].i_var) ); }
#line 2077 "compiler.tab.c"
    break;

  case 140: /* ExprValue: FLOAT_LIT  */
#line 332 "compiler.y"
                { lineType=cur_value_type=9;  fprintf(jfile, "\tldc %f ;\n",(yyvsp[0].f_var) ); }
#line 2083 "compiler.tab.c"
    break;

  case 141: /* ExprValue: BOOL_LIT  */
#line 333 "compiler.y"
               { cur_value_type=3; fprintf(jfile, "\tldc %d ;\n",(yyvsp[0].b_var) ); }
#line 2089 "compiler.tab.c"
    break;

  case 142: /* ExprValue: CHAR_LIT  */
#line 334 "compiler.y"
               { cur_value_type=11; fprintf(jfile, "\tldc \"%c\" ;\n",(yyvsp[0].c_var) ); }
#line 2095 "compiler.tab.c"
    break;

  case 143: /* ExprValue: STR_LIT  */
#line 335 "compiler.y"
              { cur_value_type=11; fprintf(jfile, "\tldc \"%s\" ;\n",(yyvsp[0].s_var) ); }
#line 2101 "compiler.tab.c"
    break;

  case 145: /* ExprValue: IDENT  */
#line 337 "compiler.y"
            { if(strcmp((yyvsp[0].s_var),"endl")==0){ cur_value_type=11; println=1; } else{ cur_value_type = find_ident_type((yyvsp[0].s_var)); cur_addr=find_ident_address((yyvsp[0].s_var)); iORf(); fprintf(jfile, "load %d\n",cur_addr); }}
#line 2107 "compiler.tab.c"
    break;


#line 2111 "compiler.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 343 "compiler.y"

/* C code section */
void init_jfile() {
    jfile = fopen("build/Main.j", "w");
    if (jfile == NULL) {
        fprintf(stderr, "Error: Could not open Main.j for writing\n");
        exit(1);
    }
    fprintf(jfile, ".source Main.j\n");
    fprintf(jfile, ".class public Main\n");
    fprintf(jfile, ".super java/lang/Object\n\n");
}

void writeFunc_jfile(char*name , char* func_sig) {
    fprintf(jfile, ".method public static %s%s\n",name,func_sig);
    fprintf(jfile, ".limit stack 100\n");
    fprintf(jfile, ".limit locals 100\n");
}

void close_jfile(){
    fprintf(jfile, "    return\n");
    fprintf(jfile, ".end method\n");
    if (jfile != NULL) {
        fclose(jfile);
    }
}

SymbolT* crt_SymbolT(int insertScope){
    printf("> Create symbol table (scope level %d)\n",insertScope);

    if(syT != NULL){
        return syT;
    }
    else{
        int size = sizeof(SymbolT);
        return (SymbolT*)malloc(size);
    }
}

void ins_SymbolT(int type,char* name,int addr,int lineno,char* func_sig,int currentScope){
    printf("> Insert `%s` (addr: %d) to scope level %d\n",name,addr,currentScope);
    
    int size = sizeof(SymbolTColumn);
    SymbolTColumn* column = (SymbolTColumn*)malloc(size);
    column->scope_level = currentScope;
    column->index = find_index(currentScope);
    column->name = name;
    column->type = type;
    column->addr = addr;
    column->lineno = lineno;
    column->func_sig = func_sig;
    column->print = 0;
    column->next = NULL;

    if(column->type == 1){
        column->type = cur_value_type;
    }

    if(tailColumn != NULL){
        tailColumn->next = column;
    }
    else{
        syT->head = column;
    }
    tailColumn = column;
} 


char* find_call(char* id){
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( strcmp(currentColumn->name,id) == 0 ){
            return currentColumn->func_sig;
        }
        currentColumn = currentColumn->next;
    }
    return "-";
}

int find_index(int currentScope){
    int index=0;
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( currentColumn->scope_level == currentScope && currentColumn->print == 0 ){
            index++;
        }
        currentColumn = currentColumn->next;
    }
    return index;
}

int find_ident_type(char* id){
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( strcmp(currentColumn->name,id) == 0 ){
            return currentColumn -> type;
        }
        currentColumn = currentColumn->next;
    }
    return 9;
}

int find_ident_address(char* id){
    int max_scope = -10;
    int return_addr = -1;
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( strcmp(currentColumn->name,id) == 0 && currentColumn->print == 0 && currentColumn->scope_level > max_scope && currentColumn->addr >= 0 ){
            max_scope = currentColumn->scope_level;
            return_addr = currentColumn -> addr;
        }
        currentColumn = currentColumn->next;
    }
    return return_addr;
}

int findTypeWithAddr(int scope_address){
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( currentColumn->addr == scope_address && currentColumn->print == 0 ){
            if(currentColumn->type >=13) currentColumn->type = stack_type[currentColumn->type];
            return currentColumn->type;
        }
        currentColumn = currentColumn->next;
    }
    return -1;
}

void ins_func_sig(char* id){
    int count=-1;
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( currentColumn->type == 0 && currentColumn->print == 0 ){
            count++;
        }

        if( strcmp(currentColumn->name,id) == 0 ){
            currentColumn->func_sig = tempFuncSig[count];
            break;
        }
        currentColumn = currentColumn->next;
    }
}

void construct_func(char* name){
    strcpy(tempFuncSig[tempFuncSig_counter],"(");
    for(int i=0 ; i < func_sig_counter ; i++){
        if(func_sig_array[i] > 12){
            strcat(tempFuncSig[tempFuncSig_counter],"[");
            func_sig_array[i] -= 12;
        }

        /*if( strcmp(name,"main") == 0 ){
           // strcat(tempFuncSig[tempFuncSig_counter],"V");
        }
        else*/ if(func_sig_array[i] == 2){
            strcat(tempFuncSig[tempFuncSig_counter],"V");
        }
        else if(func_sig_array[i] == 3){
            strcat(tempFuncSig[tempFuncSig_counter],"Z");
        }
        else if(func_sig_array[i] == 7){
            strcat(tempFuncSig[tempFuncSig_counter],"I");
        }
        else if(func_sig_array[i] == 11){
            strcat(tempFuncSig[tempFuncSig_counter],"Ljava/lang/String;");
        }

        if(i == func_sig_counter-2)        strcat(tempFuncSig[tempFuncSig_counter],")");
    }
    tempFuncSig_counter++;
    func_sig_counter = 0;
}

void print_cout(){
    if(cur_value_type>=13) cur_value_type = stack_type[cur_value_type];
    if(println==1) fprintf(jfile, "\tldc \"\\n\"\n");
    fprintf(jfile, "\tinvokevirtual java/io/PrintStream/print");
    //if(println==1) fprintf(jfile, "ln");
    fprintf(jfile, "(");
    if(cur_value_type == 2)         fprintf(jfile, "V");
    else if(cur_value_type == 7)    fprintf(jfile, "I");
    else if(cur_value_type == 9)    fprintf(jfile, "F");
    else if(cur_value_type == 3)    fprintf(jfile, "Z");
    else if(cur_value_type == 11)    fprintf(jfile, "Ljava/lang/String;");
    fprintf(jfile, ")V\n");
    println=0;
}

void iORf(){
    if(cur_value_type>=13) cur_value_type = stack_type[cur_value_type];

    if(cur_value_type==7) fprintf(jfile, "\ti");
    else if(cur_value_type==9) fprintf(jfile, "\tf");
    else if(cur_value_type==11) fprintf(jfile, "\ta");
    else if(cur_value_type==3) fprintf(jfile, "\ti");
    else fprintf(jfile, "wrong %d",cur_value_type);
}

void opGTR(){
    if(cur_value_type==7){
        fprintf(jfile, "\tif_icmpgt L%d_cmp_0\n",compare_count);
        fprintf(jfile, "\ticonst_0\n");
        fprintf(jfile, "\tgoto L%d_cmp_1\n",compare_count);
        fprintf(jfile, "L%d_cmp_0:\n",compare_count);
        fprintf(jfile, "\ticonst_1\n");
        fprintf(jfile, "L%d_cmp_1:\n",compare_count);
    }
    else if(cur_value_type==9){
        fprintf(jfile, "\tfcmpg  ;\n");
        fprintf(jfile, "\tifle L%d_cmp_0\n",compare_count);
        fprintf(jfile, "\ticonst_1\n");
        fprintf(jfile, "\tgoto L%d_cmp_1\n",compare_count);
        fprintf(jfile, "L%d_cmp_0:\n",compare_count);
        fprintf(jfile, "\ticonst_0\n");
        fprintf(jfile, "L%d_cmp_1:\n",compare_count);
    }
    compare_count++;
}

void opGEQ(){
    if(cur_value_type==7){
        fprintf(jfile, "\tif_icmpge L%d_cmp_0\n",compare_count);
        fprintf(jfile, "\ticonst_0\n");
        fprintf(jfile, "\tgoto L%d_cmp_1\n",compare_count);
        fprintf(jfile, "L%d_cmp_0:\n",compare_count);
        fprintf(jfile, "\ticonst_1\n");
        fprintf(jfile, "L%d_cmp_1:\n",compare_count);
    }
    compare_count++;
}

void opLES(){
    if(cur_value_type==7){
        fprintf(jfile, "\tif_icmplt L%d_cmp_0\n",compare_count);
        fprintf(jfile, "\ticonst_0\n");
        fprintf(jfile, "\tgoto L%d_cmp_1\n",compare_count);
        fprintf(jfile, "L%d_cmp_0:\n",compare_count);
        fprintf(jfile, "\ticonst_1\n");
        fprintf(jfile, "L%d_cmp_1:\n",compare_count);
    }
    compare_count++;
}

void opNOT(){
    fprintf(jfile, "\tifeq L%d_cmp_0\n",compare_count);
    fprintf(jfile, "\ticonst_1\n");
    fprintf(jfile, "\tgoto L%d_cmp_1\n",compare_count);
    fprintf(jfile, "L%d_cmp_0:\n",compare_count);
    fprintf(jfile, "\ticonst_0\n");
    fprintf(jfile, "L%d_cmp_1:\n",compare_count);        
    compare_count++;
}

void opNEQ(){
    fprintf(jfile, "\tif_icmpne L%d_cmp_0\n",compare_count);
    fprintf(jfile, "\ticonst_0\n");
    fprintf(jfile, "\tgoto L%d_cmp_1\n",compare_count);
    fprintf(jfile, "L%d_cmp_0:\n",compare_count);
    fprintf(jfile, "\ticonst_1\n");
    fprintf(jfile, "L%d_cmp_1:\n",compare_count);
    compare_count++;
}

void opEQL(){
    fprintf(jfile, "\tif_icmpeq L%d_cmp_0\n",compare_count);
    fprintf(jfile, "\ticonst_0\n");
    fprintf(jfile, "\tgoto L%d_cmp_1\n",compare_count);
    fprintf(jfile, "L%d_cmp_0:\n",compare_count);
    fprintf(jfile, "\ticonst_1\n");
    fprintf(jfile, "L%d_cmp_1:\n",compare_count);
    compare_count++;
}

void opBNT(){
    fprintf(jfile, "\ticonst_m1\n");
    fprintf(jfile, "\tixor\n");
}

void casting(int type){
    if(type==7){
        fprintf(jfile, "\tf2i\n");
        cur_value_type = type;
    }
    else if(type==9){
        fprintf(jfile, "\ti2f\n");
        cur_value_type = type;
    }
}
