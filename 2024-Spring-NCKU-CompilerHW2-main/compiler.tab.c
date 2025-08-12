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

    #include "compiler_common.h"
    #include "compiler_util.h"
    #include "main.h"

    int addr_no = VAR_FLAG_DEFAULT;
    int yydebug = 1;
    int scope = -1;
    int stack_type[30];
    int stack_cout[50];
    int cout_counter = 0;
    int type_no = 10;
    int array_length = 0;
    int cur_value_type = 1;
    int address = 0;
    int func_sig_array[30];
    int func_sig_counter = 0;
    char tempFuncSig[10][80];
    int tempFuncSig_counter = 0;

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

    SymbolT* syT = NULL;
    SymbolTColumn* tailColumn = NULL;

    SymbolT* crtSymbolT(int insertScope);
    void insSymbolT(SymbolT* syT,int type,char* name,int addr,int lineno,char* func_sig,int currentScope);
    void print_ident(char* name,int address);
    void print_cout();
    void printSymbolTable(SymbolTColumn* currentColumn, int currentScope);
    char* change_num_to_type(int type);
    char* printFunctionReturn(char* name,int type);
    char* find_call(SymbolT* syT,char* id);
    int find_index(SymbolT* syT,int currentScope);
    int find_ident_type(SymbolT* syT, char* id);
    int find_address(SymbolT* syT, char* id);
    void find_func_sig(SymbolT* syT, char *id);  
    void construct_func();

#line 125 "compiler.tab.c"

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
    INT = 259,                     /* INT  */
    FLOOT = 260,                   /* FLOOT  */
    BOOL = 261,                    /* BOOL  */
    STR = 262,                     /* STR  */
    SHR = 263,                     /* SHR  */
    SHL = 264,                     /* SHL  */
    BAN = 265,                     /* BAN  */
    BOR = 266,                     /* BOR  */
    BNT = 267,                     /* BNT  */
    BXO = 268,                     /* BXO  */
    ADD = 269,                     /* ADD  */
    SUB = 270,                     /* SUB  */
    MUL = 271,                     /* MUL  */
    DIV = 272,                     /* DIV  */
    REM = 273,                     /* REM  */
    NOT = 274,                     /* NOT  */
    GTR = 275,                     /* GTR  */
    LES = 276,                     /* LES  */
    GEQ = 277,                     /* GEQ  */
    LEQ = 278,                     /* LEQ  */
    EQL = 279,                     /* EQL  */
    NEQ = 280,                     /* NEQ  */
    LAN = 281,                     /* LAN  */
    LOR = 282,                     /* LOR  */
    VAL_ASSIGN = 283,              /* VAL_ASSIGN  */
    ADD_ASSIGN = 284,              /* ADD_ASSIGN  */
    SUB_ASSIGN = 285,              /* SUB_ASSIGN  */
    MUL_ASSIGN = 286,              /* MUL_ASSIGN  */
    DIV_ASSIGN = 287,              /* DIV_ASSIGN  */
    REM_ASSIGN = 288,              /* REM_ASSIGN  */
    BAN_ASSIGN = 289,              /* BAN_ASSIGN  */
    BOR_ASSIGN = 290,              /* BOR_ASSIGN  */
    BXO_ASSIGN = 291,              /* BXO_ASSIGN  */
    SHR_ASSIGN = 292,              /* SHR_ASSIGN  */
    SHL_ASSIGN = 293,              /* SHL_ASSIGN  */
    INC_ASSIGN = 294,              /* INC_ASSIGN  */
    DEC_ASSIGN = 295,              /* DEC_ASSIGN  */
    IF = 296,                      /* IF  */
    ELSE = 297,                    /* ELSE  */
    FOR = 298,                     /* FOR  */
    WHILE = 299,                   /* WHILE  */
    RETURN = 300,                  /* RETURN  */
    BREAK = 301,                   /* BREAK  */
    CONTINUE = 302,                /* CONTINUE  */
    VARIABLE_T = 303,              /* VARIABLE_T  */
    IDENT = 304,                   /* IDENT  */
    INT_LIT = 305,                 /* INT_LIT  */
    FLOAT_LIT = 306,               /* FLOAT_LIT  */
    BOOL_LIT = 307,                /* BOOL_LIT  */
    STR_LIT = 308                  /* STR_LIT  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 57 "compiler.y"

    ObjectType var_type;

    bool b_var;
    int i_var;
    float f_var;
    char *s_var;

    Object object_var;

#line 236 "compiler.tab.c"

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
  YYSYMBOL_INT = 4,                        /* INT  */
  YYSYMBOL_FLOOT = 5,                      /* FLOOT  */
  YYSYMBOL_BOOL = 6,                       /* BOOL  */
  YYSYMBOL_STR = 7,                        /* STR  */
  YYSYMBOL_SHR = 8,                        /* SHR  */
  YYSYMBOL_SHL = 9,                        /* SHL  */
  YYSYMBOL_BAN = 10,                       /* BAN  */
  YYSYMBOL_BOR = 11,                       /* BOR  */
  YYSYMBOL_BNT = 12,                       /* BNT  */
  YYSYMBOL_BXO = 13,                       /* BXO  */
  YYSYMBOL_ADD = 14,                       /* ADD  */
  YYSYMBOL_SUB = 15,                       /* SUB  */
  YYSYMBOL_MUL = 16,                       /* MUL  */
  YYSYMBOL_DIV = 17,                       /* DIV  */
  YYSYMBOL_REM = 18,                       /* REM  */
  YYSYMBOL_NOT = 19,                       /* NOT  */
  YYSYMBOL_GTR = 20,                       /* GTR  */
  YYSYMBOL_LES = 21,                       /* LES  */
  YYSYMBOL_GEQ = 22,                       /* GEQ  */
  YYSYMBOL_LEQ = 23,                       /* LEQ  */
  YYSYMBOL_EQL = 24,                       /* EQL  */
  YYSYMBOL_NEQ = 25,                       /* NEQ  */
  YYSYMBOL_LAN = 26,                       /* LAN  */
  YYSYMBOL_LOR = 27,                       /* LOR  */
  YYSYMBOL_VAL_ASSIGN = 28,                /* VAL_ASSIGN  */
  YYSYMBOL_ADD_ASSIGN = 29,                /* ADD_ASSIGN  */
  YYSYMBOL_SUB_ASSIGN = 30,                /* SUB_ASSIGN  */
  YYSYMBOL_MUL_ASSIGN = 31,                /* MUL_ASSIGN  */
  YYSYMBOL_DIV_ASSIGN = 32,                /* DIV_ASSIGN  */
  YYSYMBOL_REM_ASSIGN = 33,                /* REM_ASSIGN  */
  YYSYMBOL_BAN_ASSIGN = 34,                /* BAN_ASSIGN  */
  YYSYMBOL_BOR_ASSIGN = 35,                /* BOR_ASSIGN  */
  YYSYMBOL_BXO_ASSIGN = 36,                /* BXO_ASSIGN  */
  YYSYMBOL_SHR_ASSIGN = 37,                /* SHR_ASSIGN  */
  YYSYMBOL_SHL_ASSIGN = 38,                /* SHL_ASSIGN  */
  YYSYMBOL_INC_ASSIGN = 39,                /* INC_ASSIGN  */
  YYSYMBOL_DEC_ASSIGN = 40,                /* DEC_ASSIGN  */
  YYSYMBOL_IF = 41,                        /* IF  */
  YYSYMBOL_ELSE = 42,                      /* ELSE  */
  YYSYMBOL_FOR = 43,                       /* FOR  */
  YYSYMBOL_WHILE = 44,                     /* WHILE  */
  YYSYMBOL_RETURN = 45,                    /* RETURN  */
  YYSYMBOL_BREAK = 46,                     /* BREAK  */
  YYSYMBOL_CONTINUE = 47,                  /* CONTINUE  */
  YYSYMBOL_VARIABLE_T = 48,                /* VARIABLE_T  */
  YYSYMBOL_IDENT = 49,                     /* IDENT  */
  YYSYMBOL_INT_LIT = 50,                   /* INT_LIT  */
  YYSYMBOL_FLOAT_LIT = 51,                 /* FLOAT_LIT  */
  YYSYMBOL_BOOL_LIT = 52,                  /* BOOL_LIT  */
  YYSYMBOL_STR_LIT = 53,                   /* STR_LIT  */
  YYSYMBOL_54_ = 54,                       /* ';'  */
  YYSYMBOL_55_ = 55,                       /* '('  */
  YYSYMBOL_56_ = 56,                       /* ')'  */
  YYSYMBOL_57_ = 57,                       /* '{'  */
  YYSYMBOL_58_ = 58,                       /* '}'  */
  YYSYMBOL_59_ = 59,                       /* ','  */
  YYSYMBOL_60_ = 60,                       /* '['  */
  YYSYMBOL_61_ = 61,                       /* ']'  */
  YYSYMBOL_62_ = 62,                       /* ':'  */
  YYSYMBOL_YYACCEPT = 63,                  /* $accept  */
  YYSYMBOL_Program = 64,                   /* Program  */
  YYSYMBOL_65_1 = 65,                      /* $@1  */
  YYSYMBOL_GlobalStmtList = 66,            /* GlobalStmtList  */
  YYSYMBOL_GlobalStmt = 67,                /* GlobalStmt  */
  YYSYMBOL_DefineVariableStmt = 68,        /* DefineVariableStmt  */
  YYSYMBOL_FunctionDefStmt = 69,           /* FunctionDefStmt  */
  YYSYMBOL_70_2 = 70,                      /* $@2  */
  YYSYMBOL_71_3 = 71,                      /* $@3  */
  YYSYMBOL_72_4 = 72,                      /* $@4  */
  YYSYMBOL_FunctionParameterStmtList = 73, /* FunctionParameterStmtList  */
  YYSYMBOL_FunctionParameterStmt = 74,     /* FunctionParameterStmt  */
  YYSYMBOL_StmtList = 75,                  /* StmtList  */
  YYSYMBOL_Stmt = 76,                      /* Stmt  */
  YYSYMBOL_77_5 = 77,                      /* $@5  */
  YYSYMBOL_78_6 = 78,                      /* $@6  */
  YYSYMBOL_79_7 = 79,                      /* $@7  */
  YYSYMBOL_80_8 = 80,                      /* $@8  */
  YYSYMBOL_81_9 = 81,                      /* $@9  */
  YYSYMBOL_IfStmt = 82,                    /* IfStmt  */
  YYSYMBOL_83_10 = 83,                     /* $@10  */
  YYSYMBOL_RegularDefineStmt = 84,         /* RegularDefineStmt  */
  YYSYMBOL_ForStmt = 85,                   /* ForStmt  */
  YYSYMBOL_86_11 = 86,                     /* $@11  */
  YYSYMBOL_ForDefineStmt = 87,             /* ForDefineStmt  */
  YYSYMBOL_ArrayStmt = 88,                 /* ArrayStmt  */
  YYSYMBOL_ArrayVariable = 89,             /* ArrayVariable  */
  YYSYMBOL_ArrayExpression = 90,           /* ArrayExpression  */
  YYSYMBOL_ArrayInsideExpression = 91,     /* ArrayInsideExpression  */
  YYSYMBOL_StmtParameter = 92,             /* StmtParameter  */
  YYSYMBOL_StmtParameterList = 93,         /* StmtParameterList  */
  YYSYMBOL_CoutParmListStmt = 94,          /* CoutParmListStmt  */
  YYSYMBOL_Expr = 95,                      /* Expr  */
  YYSYMBOL_ExprRightSide = 96,             /* ExprRightSide  */
  YYSYMBOL_ExprDoubleAnd = 97,             /* ExprDoubleAnd  */
  YYSYMBOL_ExprOr = 98,                    /* ExprOr  */
  YYSYMBOL_ExprXor = 99,                   /* ExprXor  */
  YYSYMBOL_ExprAnd = 100,                  /* ExprAnd  */
  YYSYMBOL_ExprSame = 101,                 /* ExprSame  */
  YYSYMBOL_ExprBigOrSmall = 102,           /* ExprBigOrSmall  */
  YYSYMBOL_ExprBitwise = 103,              /* ExprBitwise  */
  YYSYMBOL_ExprPlus = 104,                 /* ExprPlus  */
  YYSYMBOL_ExprCross = 105,                /* ExprCross  */
  YYSYMBOL_ExprFrontUnary = 106,           /* ExprFrontUnary  */
  YYSYMBOL_ExprPostfix = 107,              /* ExprPostfix  */
  YYSYMBOL_ExprValue = 108,                /* ExprValue  */
  YYSYMBOL_ExprLeftSide = 109              /* ExprLeftSide  */
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
#define YYLAST   466

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  63
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  47
/* YYNRULES -- Number of rules.  */
#define YYNRULES  118
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  220

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   308


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
      55,    56,     2,     2,    59,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    62,    54,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    60,     2,    61,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    57,     2,    58,     2,     2,     2,     2,
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
      45,    46,    47,    48,    49,    50,    51,    52,    53
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   110,   110,   110,   114,   115,   119,   120,   124,   129,
     129,   129,   129,   133,   134,   138,   139,   140,   145,   146,
     150,   151,   152,   153,   154,   154,   155,   155,   156,   156,
     156,   157,   157,   158,   159,   160,   164,   165,   165,   169,
     170,   171,   175,   176,   176,   180,   181,   185,   186,   190,
     191,   195,   196,   200,   201,   202,   206,   207,   211,   212,
     216,   217,   221,   222,   223,   224,   225,   226,   227,   228,
     229,   230,   231,   235,   236,   240,   241,   245,   246,   250,
     251,   255,   256,   260,   261,   262,   266,   267,   268,   269,
     270,   274,   275,   276,   280,   281,   282,   286,   287,   288,
     289,   293,   294,   295,   296,   297,   298,   302,   303,   304,
     308,   309,   310,   311,   312,   313,   314,   318,   319
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
  "\"end of file\"", "error", "\"invalid token\"", "COUT", "INT", "FLOOT",
  "BOOL", "STR", "SHR", "SHL", "BAN", "BOR", "BNT", "BXO", "ADD", "SUB",
  "MUL", "DIV", "REM", "NOT", "GTR", "LES", "GEQ", "LEQ", "EQL", "NEQ",
  "LAN", "LOR", "VAL_ASSIGN", "ADD_ASSIGN", "SUB_ASSIGN", "MUL_ASSIGN",
  "DIV_ASSIGN", "REM_ASSIGN", "BAN_ASSIGN", "BOR_ASSIGN", "BXO_ASSIGN",
  "SHR_ASSIGN", "SHL_ASSIGN", "INC_ASSIGN", "DEC_ASSIGN", "IF", "ELSE",
  "FOR", "WHILE", "RETURN", "BREAK", "CONTINUE", "VARIABLE_T", "IDENT",
  "INT_LIT", "FLOAT_LIT", "BOOL_LIT", "STR_LIT", "';'", "'('", "')'",
  "'{'", "'}'", "','", "'['", "']'", "':'", "$accept", "Program", "$@1",
  "GlobalStmtList", "GlobalStmt", "DefineVariableStmt", "FunctionDefStmt",
  "$@2", "$@3", "$@4", "FunctionParameterStmtList",
  "FunctionParameterStmt", "StmtList", "Stmt", "$@5", "$@6", "$@7", "$@8",
  "$@9", "IfStmt", "$@10", "RegularDefineStmt", "ForStmt", "$@11",
  "ForDefineStmt", "ArrayStmt", "ArrayVariable", "ArrayExpression",
  "ArrayInsideExpression", "StmtParameter", "StmtParameterList",
  "CoutParmListStmt", "Expr", "ExprRightSide", "ExprDoubleAnd", "ExprOr",
  "ExprXor", "ExprAnd", "ExprSame", "ExprBigOrSmall", "ExprBitwise",
  "ExprPlus", "ExprCross", "ExprFrontUnary", "ExprPostfix", "ExprValue",
  "ExprLeftSide", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-143)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-44)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -143,     4,    25,  -143,   -18,    25,  -143,  -143,  -143,    48,
    -143,   411,    23,   411,   411,   411,   411,   -39,  -143,  -143,
    -143,  -143,   345,   -24,    70,    92,    94,   103,    17,    46,
      16,    57,    21,  -143,  -143,    55,  -143,  -143,  -143,  -143,
    -143,  -143,   353,    52,    63,    59,    60,    97,   403,   411,
    -143,   411,   411,   411,   411,   411,   411,   411,   411,   411,
     411,   411,   411,   411,   411,   411,   411,   411,  -143,  -143,
     353,    76,   -36,  -143,    57,  -143,  -143,    65,    52,   170,
    -143,   411,   411,   411,   411,   411,   411,   411,   411,   411,
     411,    70,    92,    94,   103,    17,    46,    46,    16,    16,
      16,    16,    57,    57,    21,    21,  -143,  -143,  -143,   -27,
      69,  -143,   353,  -143,    72,   411,  -143,    97,    97,    97,
      97,    97,    97,    97,    97,    97,    97,  -143,    78,  -143,
    -143,    73,  -143,   301,   122,    86,  -143,  -143,  -143,   361,
      89,    99,  -143,     3,  -143,    95,  -143,   411,     5,   411,
      93,   100,   102,  -143,    98,  -143,   -20,   101,  -143,  -143,
    -143,  -143,    57,   411,  -143,   105,   301,   369,   411,  -143,
     411,     0,   109,    57,  -143,   159,   110,  -143,   108,   111,
    -143,    97,    85,   126,  -143,   301,  -143,   -26,   112,   411,
     114,   411,   121,    57,  -143,  -143,   123,   104,    63,   301,
     125,   124,    47,    57,  -143,   301,   411,   212,   411,   301,
    -143,   411,   230,  -143,  -143,  -143,   283,    57,  -143,  -143
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       2,     0,     0,     1,     0,     3,     5,     6,     7,     9,
       4,     0,     0,     0,     0,     0,     0,   117,   110,   111,
     112,   113,     0,     0,    74,    76,    78,    80,    82,    85,
      90,    93,    96,   100,   106,   109,   116,    10,   102,   103,
     104,   101,     0,     0,   118,     0,     0,    72,   116,     0,
       8,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,   108,
       0,     0,     0,    14,    17,    49,    50,     0,     0,     0,
     115,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    75,    77,    79,    81,    83,    84,    87,    89,
      86,    88,    91,    92,    94,    95,    97,    98,    99,     0,
      15,   114,     0,    48,     0,     0,   105,    62,    63,    64,
      65,    66,    67,    70,    71,    68,    69,    11,     0,    13,
      47,     0,    16,     0,     0,     0,    26,    31,    28,     0,
       0,     0,    20,     0,    19,     0,    41,     0,     0,     0,
       0,     0,     0,    34,     0,    33,    59,    39,    57,    12,
      18,    21,    61,     0,    22,     0,     0,    46,     0,    35,
       0,    40,     0,    60,    24,     0,     0,    45,     0,     0,
      29,    58,     0,    59,    56,    37,    27,    59,     0,     0,
       0,    55,     0,    52,    36,    25,     0,     0,    40,     0,
       0,     0,     0,    54,    23,     0,     0,     0,     0,     0,
      51,     0,     0,    44,    32,    42,     0,    53,    38,    30
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -143,  -143,  -143,  -143,   177,  -143,  -143,  -143,  -143,  -143,
     118,    80,   -91,  -142,  -143,  -143,  -143,  -143,  -143,  -143,
    -143,    26,  -143,  -143,  -143,  -137,   117,  -143,  -143,  -143,
      24,  -143,   -17,    -2,   148,   147,   154,   146,   162,    54,
      32,   -35,    56,    -3,   139,  -143,   -22
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     1,     2,     5,     6,     7,     8,    12,    70,   131,
      72,    73,   143,   144,   185,   150,   152,   190,   151,   195,
     196,   145,   178,   197,   179,    44,    77,   192,   202,   157,
     158,   148,   146,    47,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      48,   160,   170,    49,     3,    46,   134,    74,   170,    23,
      38,    39,    40,    41,   163,    13,    42,    14,    15,   171,
     111,    43,    16,   112,    61,    62,   102,   103,   182,   127,
      50,     9,   112,   160,    43,    74,   -43,    65,    66,    67,
      43,    55,    56,   194,   135,   136,   137,   138,   139,   140,
     198,   141,    17,    18,    19,    20,    21,   142,    22,   164,
      78,   159,   106,   107,   108,   160,    57,    58,    59,    60,
     160,    63,    64,     4,   160,   175,    11,    74,    37,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    98,
      99,   100,   101,    48,    68,    69,    51,    13,    46,    14,
      15,    75,    76,    52,    16,   210,   211,    53,   207,    96,
      97,    48,   162,    54,   212,    79,    80,    48,   216,   104,
     105,    48,   154,    78,    49,   110,   113,    48,   173,   128,
     133,   147,   165,   130,    17,    18,    19,    20,    21,   132,
      22,   149,   191,   155,    48,    48,    48,   193,   156,   161,
     166,   180,   169,    48,   170,   167,   203,   168,   183,   187,
     172,   174,   134,    48,   188,   189,   206,    48,   181,   199,
     201,    13,   200,    14,    15,   204,   217,    48,    16,   208,
     205,   209,    10,    48,    48,    48,    48,    48,   109,   213,
      48,   215,   129,   177,    48,   114,   184,    91,    92,    94,
     135,   136,   137,   138,   139,   140,    93,   141,    17,    18,
      19,    20,    21,   142,    22,   134,    95,   186,   116,    17,
      18,    19,    20,    21,    13,   115,    14,    15,     0,     0,
       0,    16,     0,   134,     0,     0,     0,     0,     0,     0,
       0,     0,    13,     0,    14,    15,     0,     0,     0,    16,
       0,     0,     0,   135,   136,   137,   138,   139,   140,     0,
     141,    17,    18,    19,    20,    21,   142,    22,     0,     0,
     214,   135,   136,   137,   138,   139,   140,     0,   141,    17,
      18,    19,    20,    21,   142,    22,   134,     0,   218,     0,
       0,     0,     0,     0,     0,    13,     0,    14,    15,     0,
       0,     0,    16,     0,   134,     0,     0,     0,     0,     0,
       0,     0,     0,    13,     0,    14,    15,     0,     0,     0,
      16,     0,     0,     0,   135,   136,   137,   138,   139,   140,
       0,   141,    17,    18,    19,    20,    21,   142,    22,     0,
       0,   219,   135,   136,   137,   138,   139,   140,     0,   141,
      17,    18,    19,    20,    21,   142,    22,    13,     0,    14,
      15,     0,     0,     0,    16,    13,     0,    14,    15,     0,
       0,     0,    16,    13,     0,    14,    15,     0,     0,     0,
      16,    13,     0,    14,    15,     0,     0,     0,    16,     0,
       0,     0,     0,    45,    17,    18,    19,    20,    21,     0,
      22,    71,    17,    18,    19,    20,    21,     0,    22,     0,
      17,    18,    19,    20,    21,   153,    22,   176,    17,    18,
      19,    20,    21,    13,    22,    14,    15,     0,     0,     0,
      16,    81,    82,    83,    84,    85,    86,    87,    88,     0,
      89,    90,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      17,    18,    19,    20,    21,     0,    22
};

static const yytype_int16 yycheck[] =
{
      22,   143,    28,    27,     0,    22,     3,    42,    28,    11,
      13,    14,    15,    16,     9,    12,    55,    14,    15,   156,
      56,    60,    19,    59,     8,     9,    61,    62,    28,    56,
      54,    49,    59,   175,    60,    70,    62,    16,    17,    18,
      60,    24,    25,   185,    41,    42,    43,    44,    45,    46,
     187,    48,    49,    50,    51,    52,    53,    54,    55,    54,
      60,    58,    65,    66,    67,   207,    20,    21,    22,    23,
     212,    14,    15,    48,   216,   166,    28,   112,    55,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    57,
      58,    59,    60,   115,    39,    40,    26,    12,   115,    14,
      15,    49,    50,    11,    19,    58,    59,    13,   199,    55,
      56,   133,   147,    10,   205,    56,    56,   139,   209,    63,
      64,   143,   139,    60,    27,    49,    61,   149,   163,    60,
      57,     9,   149,    61,    49,    50,    51,    52,    53,    61,
      55,    55,    57,    54,   166,   167,   168,   182,    49,    54,
      57,   168,    54,   175,    28,    55,   191,    55,    49,    49,
      59,    56,     3,   185,    56,    54,    62,   189,   170,    57,
      56,    12,   189,    14,    15,    54,   211,   199,    19,    54,
      57,    57,     5,   205,   206,   207,   208,   209,    70,   206,
     212,   208,   112,   167,   216,    78,   172,    49,    51,    53,
      41,    42,    43,    44,    45,    46,    52,    48,    49,    50,
      51,    52,    53,    54,    55,     3,    54,    58,    79,    49,
      50,    51,    52,    53,    12,    55,    14,    15,    -1,    -1,
      -1,    19,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    -1,    14,    15,    -1,    -1,    -1,    19,
      -1,    -1,    -1,    41,    42,    43,    44,    45,    46,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    -1,    -1,
      58,    41,    42,    43,    44,    45,    46,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,     3,    -1,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    -1,    14,    15,    -1,
      -1,    -1,    19,    -1,     3,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    -1,    14,    15,    -1,    -1,    -1,
      19,    -1,    -1,    -1,    41,    42,    43,    44,    45,    46,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    -1,
      -1,    58,    41,    42,    43,    44,    45,    46,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    12,    -1,    14,
      15,    -1,    -1,    -1,    19,    12,    -1,    14,    15,    -1,
      -1,    -1,    19,    12,    -1,    14,    15,    -1,    -1,    -1,
      19,    12,    -1,    14,    15,    -1,    -1,    -1,    19,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    -1,
      55,    48,    49,    50,    51,    52,    53,    -1,    55,    -1,
      49,    50,    51,    52,    53,    54,    55,    48,    49,    50,
      51,    52,    53,    12,    55,    14,    15,    -1,    -1,    -1,
      19,    28,    29,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    50,    51,    52,    53,    -1,    55
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    64,    65,     0,    48,    66,    67,    68,    69,    49,
      67,    28,    70,    12,    14,    15,    19,    49,    50,    51,
      52,    53,    55,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,    55,   106,   106,
     106,   106,    55,    60,    88,    48,    95,    96,   109,    27,
      54,    26,    11,    13,    10,    24,    25,    20,    21,    22,
      23,     8,     9,    14,    15,    16,    17,    18,    39,    40,
      71,    48,    73,    74,   104,    49,    50,    89,    60,    56,
      56,    28,    29,    30,    31,    32,    33,    34,    35,    37,
      38,    97,    98,    99,   100,   101,   102,   102,   103,   103,
     103,   103,   104,   104,   105,   105,   106,   106,   106,    73,
      49,    56,    59,    61,    89,    55,   107,    96,    96,    96,
      96,    96,    96,    96,    96,    96,    96,    56,    60,    74,
      61,    72,    61,    57,     3,    41,    42,    43,    44,    45,
      46,    48,    54,    75,    76,    84,    95,     9,    94,    55,
      78,    81,    79,    54,    95,    54,    49,    92,    93,    58,
      76,    54,   104,     9,    54,    95,    57,    55,    55,    54,
      28,    88,    59,   104,    56,    75,    48,    84,    85,    87,
      95,    96,    28,    49,    93,    77,    58,    49,    56,    54,
      80,    57,    90,   104,    76,    82,    83,    86,    88,    57,
      95,    56,    91,   104,    54,    57,    62,    75,    54,    57,
      58,    59,    75,    95,    58,    95,    75,   104,    58,    58
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    63,    65,    64,    66,    66,    67,    67,    68,    70,
      71,    72,    69,    73,    73,    74,    74,    74,    75,    75,
      76,    76,    76,    76,    77,    76,    78,    76,    79,    80,
      76,    81,    76,    76,    76,    76,    82,    83,    82,    84,
      84,    84,    85,    86,    85,    87,    87,    88,    88,    89,
      89,    90,    90,    91,    91,    91,    92,    92,    93,    93,
      94,    94,    95,    95,    95,    95,    95,    95,    95,    95,
      95,    95,    95,    96,    96,    97,    97,    98,    98,    99,
      99,   100,   100,   101,   101,   101,   102,   102,   102,   102,
     102,   103,   103,   103,   104,   104,   104,   105,   105,   105,
     105,   106,   106,   106,   106,   106,   106,   107,   107,   107,
     108,   108,   108,   108,   108,   108,   108,   109,   109
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     1,     5,     0,
       0,     0,    11,     3,     1,     2,     4,     1,     2,     1,
       1,     2,     3,     6,     0,     6,     0,     5,     0,     0,
       9,     0,     8,     2,     2,     3,     1,     0,     4,     2,
       3,     1,     5,     0,     5,     1,     0,     4,     3,     1,
       1,     3,     1,     3,     1,     0,     3,     1,     3,     1,
       3,     2,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     1,     3,     1,     3,     1,     3,     1,     3,
       1,     3,     1,     3,     3,     1,     3,     3,     3,     3,
       1,     3,     3,     1,     3,     3,     1,     3,     3,     3,
       1,     2,     2,     2,     2,     4,     1,     2,     2,     1,
       1,     1,     1,     1,     4,     3,     1,     1,     2
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
#line 110 "compiler.y"
      { syT=crtSymbolT(++scope);}
#line 1528 "compiler.tab.c"
    break;

  case 3: /* Program: $@1 GlobalStmtList  */
#line 110 "compiler.y"
                                                 { printSymbolTable(syT->head,scope--); }
#line 1534 "compiler.tab.c"
    break;

  case 9: /* $@2: %empty  */
#line 129 "compiler.y"
                       { printf("func: %s\n",(yyvsp[0].s_var)); insSymbolT(syT,0,(yyvsp[0].s_var),-1,yylineno,"unknown",scope); }
#line 1540 "compiler.tab.c"
    break;

  case 10: /* $@3: %empty  */
#line 129 "compiler.y"
                                                                                                                        { syT=crtSymbolT(++scope); }
#line 1546 "compiler.tab.c"
    break;

  case 11: /* $@4: %empty  */
#line 129 "compiler.y"
                                                                                                                                                                                   { func_sig_array[func_sig_counter++]=(yyvsp[-6].var_type); construct_func(); find_func_sig(syT,(yyvsp[-5].s_var)); }
#line 1552 "compiler.tab.c"
    break;

  case 12: /* FunctionDefStmt: VARIABLE_T IDENT $@2 '(' $@3 FunctionParameterStmtList ')' $@4 '{' StmtList '}'  */
#line 129 "compiler.y"
                                                                                                                                                                                                                                                                                                         { printSymbolTable(syT->head,scope--); }
#line 1558 "compiler.tab.c"
    break;

  case 15: /* FunctionParameterStmt: VARIABLE_T IDENT  */
#line 138 "compiler.y"
                       { insSymbolT(syT,(yyvsp[-1].var_type),(yyvsp[0].s_var),address++,yylineno,"-",scope); func_sig_array[func_sig_counter++]=(yyvsp[-1].var_type); }
#line 1564 "compiler.tab.c"
    break;

  case 16: /* FunctionParameterStmt: VARIABLE_T IDENT '[' ']'  */
#line 139 "compiler.y"
                               { insSymbolT(syT,(yyvsp[-3].var_type),(yyvsp[-2].s_var),address++,yylineno,"-",scope); func_sig_array[func_sig_counter++]=((yyvsp[-3].var_type)+10); }
#line 1570 "compiler.tab.c"
    break;

  case 22: /* Stmt: COUT CoutParmListStmt ';'  */
#line 152 "compiler.y"
                                { print_cout(); }
#line 1576 "compiler.tab.c"
    break;

  case 23: /* Stmt: VARIABLE_T IDENT ArrayStmt VAL_ASSIGN ArrayExpression ';'  */
#line 153 "compiler.y"
                                                                { insSymbolT(syT,(yyvsp[-5].var_type),(yyvsp[-4].s_var),address++,yylineno,"-",scope); }
#line 1582 "compiler.tab.c"
    break;

  case 24: /* $@5: %empty  */
#line 154 "compiler.y"
                      { printf("IF\n"); }
#line 1588 "compiler.tab.c"
    break;

  case 26: /* $@6: %empty  */
#line 155 "compiler.y"
           { printf("ELSE\n"); syT=crtSymbolT(++scope); }
#line 1594 "compiler.tab.c"
    break;

  case 27: /* Stmt: ELSE $@6 '{' StmtList '}'  */
#line 155 "compiler.y"
                                                                           { printSymbolTable(syT->head,scope); scope--;}
#line 1600 "compiler.tab.c"
    break;

  case 28: /* $@7: %empty  */
#line 156 "compiler.y"
            { printf("WHILE\n"); }
#line 1606 "compiler.tab.c"
    break;

  case 29: /* $@8: %empty  */
#line 156 "compiler.y"
                                            { syT=crtSymbolT(++scope); }
#line 1612 "compiler.tab.c"
    break;

  case 30: /* Stmt: WHILE $@7 '(' Expr $@8 ')' '{' StmtList '}'  */
#line 156 "compiler.y"
                                                                                              { printSymbolTable(syT->head,scope--);}
#line 1618 "compiler.tab.c"
    break;

  case 31: /* $@9: %empty  */
#line 157 "compiler.y"
          { printf("FOR\n"); syT=crtSymbolT(++scope); }
#line 1624 "compiler.tab.c"
    break;

  case 32: /* Stmt: FOR $@9 '(' ForStmt ')' '{' StmtList '}'  */
#line 157 "compiler.y"
                                                                                         { printSymbolTable(syT->head,scope--);}
#line 1630 "compiler.tab.c"
    break;

  case 33: /* Stmt: BREAK ';'  */
#line 158 "compiler.y"
                { printf("BREAK\n"); }
#line 1636 "compiler.tab.c"
    break;

  case 34: /* Stmt: RETURN ';'  */
#line 159 "compiler.y"
                 { printf("RETURN\n"); }
#line 1642 "compiler.tab.c"
    break;

  case 35: /* Stmt: RETURN Expr ';'  */
#line 160 "compiler.y"
                      { printf("RETURN\n"); }
#line 1648 "compiler.tab.c"
    break;

  case 37: /* $@10: %empty  */
#line 165 "compiler.y"
      { syT=crtSymbolT(++scope); }
#line 1654 "compiler.tab.c"
    break;

  case 38: /* IfStmt: $@10 '{' StmtList '}'  */
#line 165 "compiler.y"
                                                    { printSymbolTable(syT->head,scope--);}
#line 1660 "compiler.tab.c"
    break;

  case 39: /* RegularDefineStmt: VARIABLE_T StmtParameter  */
#line 169 "compiler.y"
                               { if((yyvsp[-1].var_type)!=1){ cur_value_type=(yyvsp[-1].var_type); } stack_type[type_no++]=cur_value_type; }
#line 1666 "compiler.tab.c"
    break;

  case 40: /* RegularDefineStmt: VARIABLE_T IDENT ArrayStmt  */
#line 170 "compiler.y"
                                 { insSymbolT(syT,(yyvsp[-2].var_type),(yyvsp[-1].s_var),address++,yylineno,"-",scope); }
#line 1672 "compiler.tab.c"
    break;

  case 43: /* $@11: %empty  */
#line 176 "compiler.y"
                       { insSymbolT(syT,(yyvsp[-1].var_type),(yyvsp[0].s_var),address++,yylineno,"-",scope); }
#line 1678 "compiler.tab.c"
    break;

  case 49: /* ArrayVariable: IDENT  */
#line 190 "compiler.y"
            { print_ident((yyvsp[0].s_var),find_address(syT,(yyvsp[0].s_var))); }
#line 1684 "compiler.tab.c"
    break;

  case 50: /* ArrayVariable: INT_LIT  */
#line 191 "compiler.y"
              { printf("INT_LIT %d\n",(yyvsp[0].i_var)); }
#line 1690 "compiler.tab.c"
    break;

  case 51: /* ArrayExpression: '{' ArrayInsideExpression '}'  */
#line 195 "compiler.y"
                                    { printf("create array: %d\n",array_length); array_length=0; }
#line 1696 "compiler.tab.c"
    break;

  case 53: /* ArrayInsideExpression: ArrayInsideExpression ',' ExprPlus  */
#line 200 "compiler.y"
                                         { array_length++; }
#line 1702 "compiler.tab.c"
    break;

  case 54: /* ArrayInsideExpression: ExprPlus  */
#line 201 "compiler.y"
               { array_length++; }
#line 1708 "compiler.tab.c"
    break;

  case 58: /* StmtParameterList: IDENT VAL_ASSIGN ExprRightSide  */
#line 211 "compiler.y"
                                     { insSymbolT(syT,type_no,(yyvsp[-2].s_var),address++,yylineno,"-",scope); }
#line 1714 "compiler.tab.c"
    break;

  case 59: /* StmtParameterList: IDENT  */
#line 212 "compiler.y"
            { insSymbolT(syT,type_no,(yyvsp[0].s_var),address++,yylineno,"-",scope); }
#line 1720 "compiler.tab.c"
    break;

  case 60: /* CoutParmListStmt: CoutParmListStmt SHL ExprPlus  */
#line 216 "compiler.y"
                                    { stack_cout[cout_counter++]=cur_value_type; }
#line 1726 "compiler.tab.c"
    break;

  case 61: /* CoutParmListStmt: SHL ExprPlus  */
#line 217 "compiler.y"
                   { stack_cout[cout_counter++]=cur_value_type; }
#line 1732 "compiler.tab.c"
    break;

  case 62: /* Expr: ExprLeftSide VAL_ASSIGN ExprRightSide  */
#line 221 "compiler.y"
                                            { printf("EQL_ASSIGN\n"); }
#line 1738 "compiler.tab.c"
    break;

  case 63: /* Expr: ExprLeftSide ADD_ASSIGN ExprRightSide  */
#line 222 "compiler.y"
                                            { printf("ADD_ASSIGN\n"); }
#line 1744 "compiler.tab.c"
    break;

  case 64: /* Expr: ExprLeftSide SUB_ASSIGN ExprRightSide  */
#line 223 "compiler.y"
                                            { printf("SUB_ASSIGN\n"); }
#line 1750 "compiler.tab.c"
    break;

  case 65: /* Expr: ExprLeftSide MUL_ASSIGN ExprRightSide  */
#line 224 "compiler.y"
                                            { printf("MUL_ASSIGN\n"); }
#line 1756 "compiler.tab.c"
    break;

  case 66: /* Expr: ExprLeftSide DIV_ASSIGN ExprRightSide  */
#line 225 "compiler.y"
                                            { printf("DIV_ASSIGN\n"); }
#line 1762 "compiler.tab.c"
    break;

  case 67: /* Expr: ExprLeftSide REM_ASSIGN ExprRightSide  */
#line 226 "compiler.y"
                                            { printf("REM_ASSIGN\n"); }
#line 1768 "compiler.tab.c"
    break;

  case 68: /* Expr: ExprLeftSide SHR_ASSIGN ExprRightSide  */
#line 227 "compiler.y"
                                            { printf("SHR_ASSIGN\n"); }
#line 1774 "compiler.tab.c"
    break;

  case 69: /* Expr: ExprLeftSide SHL_ASSIGN ExprRightSide  */
#line 228 "compiler.y"
                                            { printf("SHL_ASSIGN\n"); }
#line 1780 "compiler.tab.c"
    break;

  case 70: /* Expr: ExprLeftSide BAN_ASSIGN ExprRightSide  */
#line 229 "compiler.y"
                                            { printf("BAN_ASSIGN\n"); }
#line 1786 "compiler.tab.c"
    break;

  case 71: /* Expr: ExprLeftSide BOR_ASSIGN ExprRightSide  */
#line 230 "compiler.y"
                                            { printf("BOR_ASSIGN\n"); }
#line 1792 "compiler.tab.c"
    break;

  case 73: /* ExprRightSide: ExprRightSide LOR ExprDoubleAnd  */
#line 235 "compiler.y"
                                      { printf("LOR\n"); }
#line 1798 "compiler.tab.c"
    break;

  case 75: /* ExprDoubleAnd: ExprDoubleAnd LAN ExprOr  */
#line 240 "compiler.y"
                               { printf("LAN\n"); }
#line 1804 "compiler.tab.c"
    break;

  case 77: /* ExprOr: ExprOr BOR ExprXor  */
#line 245 "compiler.y"
                         { printf("BOR\n"); }
#line 1810 "compiler.tab.c"
    break;

  case 79: /* ExprXor: ExprXor BXO ExprAnd  */
#line 250 "compiler.y"
                          { printf("BXO\n"); }
#line 1816 "compiler.tab.c"
    break;

  case 81: /* ExprAnd: ExprAnd BAN ExprSame  */
#line 255 "compiler.y"
                           { printf("BAN\n"); }
#line 1822 "compiler.tab.c"
    break;

  case 83: /* ExprSame: ExprSame EQL ExprBigOrSmall  */
#line 260 "compiler.y"
                                  { printf("EQL\n"); }
#line 1828 "compiler.tab.c"
    break;

  case 84: /* ExprSame: ExprSame NEQ ExprBigOrSmall  */
#line 261 "compiler.y"
                                  { printf("NEQ\n"); }
#line 1834 "compiler.tab.c"
    break;

  case 86: /* ExprBigOrSmall: ExprBigOrSmall GEQ ExprBitwise  */
#line 266 "compiler.y"
                                     { printf("GEQ\n"); }
#line 1840 "compiler.tab.c"
    break;

  case 87: /* ExprBigOrSmall: ExprBigOrSmall GTR ExprBitwise  */
#line 267 "compiler.y"
                                     { printf("GTR\n"); }
#line 1846 "compiler.tab.c"
    break;

  case 88: /* ExprBigOrSmall: ExprBigOrSmall LEQ ExprBitwise  */
#line 268 "compiler.y"
                                     { printf("LEQ\n"); }
#line 1852 "compiler.tab.c"
    break;

  case 89: /* ExprBigOrSmall: ExprBigOrSmall LES ExprBitwise  */
#line 269 "compiler.y"
                                     { printf("LES\n"); }
#line 1858 "compiler.tab.c"
    break;

  case 91: /* ExprBitwise: ExprBitwise SHR ExprPlus  */
#line 274 "compiler.y"
                               { printf("SHR\n"); }
#line 1864 "compiler.tab.c"
    break;

  case 92: /* ExprBitwise: ExprBitwise SHL ExprPlus  */
#line 275 "compiler.y"
                               { printf("SHL\n"); }
#line 1870 "compiler.tab.c"
    break;

  case 94: /* ExprPlus: ExprPlus ADD ExprCross  */
#line 280 "compiler.y"
                             { printf("ADD\n"); }
#line 1876 "compiler.tab.c"
    break;

  case 95: /* ExprPlus: ExprPlus SUB ExprCross  */
#line 281 "compiler.y"
                             { printf("SUB\n"); }
#line 1882 "compiler.tab.c"
    break;

  case 97: /* ExprCross: ExprCross MUL ExprFrontUnary  */
#line 286 "compiler.y"
                                   { printf("MUL\n"); }
#line 1888 "compiler.tab.c"
    break;

  case 98: /* ExprCross: ExprCross DIV ExprFrontUnary  */
#line 287 "compiler.y"
                                   { printf("DIV\n"); }
#line 1894 "compiler.tab.c"
    break;

  case 99: /* ExprCross: ExprCross REM ExprFrontUnary  */
#line 288 "compiler.y"
                                   { printf("REM\n"); }
#line 1900 "compiler.tab.c"
    break;

  case 101: /* ExprFrontUnary: NOT ExprFrontUnary  */
#line 293 "compiler.y"
                         { printf("NOT\n"); }
#line 1906 "compiler.tab.c"
    break;

  case 102: /* ExprFrontUnary: BNT ExprFrontUnary  */
#line 294 "compiler.y"
                         { printf("BNT\n"); }
#line 1912 "compiler.tab.c"
    break;

  case 103: /* ExprFrontUnary: ADD ExprFrontUnary  */
#line 295 "compiler.y"
                         { printf("ADD\n"); }
#line 1918 "compiler.tab.c"
    break;

  case 104: /* ExprFrontUnary: SUB ExprFrontUnary  */
#line 296 "compiler.y"
                         { printf("NEG\n"); }
#line 1924 "compiler.tab.c"
    break;

  case 105: /* ExprFrontUnary: '(' VARIABLE_T ')' ExprPostfix  */
#line 297 "compiler.y"
                                     { printf("Cast to %s\n", change_num_to_type((yyvsp[-2].var_type))); }
#line 1930 "compiler.tab.c"
    break;

  case 107: /* ExprPostfix: ExprValue INC_ASSIGN  */
#line 302 "compiler.y"
                           { printf("INC_ASSIGN\n"); }
#line 1936 "compiler.tab.c"
    break;

  case 108: /* ExprPostfix: ExprValue DEC_ASSIGN  */
#line 303 "compiler.y"
                           { printf("DEC_ASSIGN\n"); }
#line 1942 "compiler.tab.c"
    break;

  case 110: /* ExprValue: INT_LIT  */
#line 308 "compiler.y"
              { printf("INT_LIT %d\n",(yyvsp[0].i_var)); cur_value_type = 4; }
#line 1948 "compiler.tab.c"
    break;

  case 111: /* ExprValue: FLOAT_LIT  */
#line 309 "compiler.y"
                { printf("FLOAT_LIT %f\n",(yyvsp[0].f_var)); cur_value_type = 6; }
#line 1954 "compiler.tab.c"
    break;

  case 112: /* ExprValue: BOOL_LIT  */
#line 310 "compiler.y"
               { printf("BOOL_LIT "); ((yyvsp[0].b_var)==0)?printf("FALSE\n"):printf("TRUE\n"); cur_value_type = 8; }
#line 1960 "compiler.tab.c"
    break;

  case 113: /* ExprValue: STR_LIT  */
#line 311 "compiler.y"
              { printf("STR_LIT \"%s\"\n",(yyvsp[0].s_var)); cur_value_type = 9; }
#line 1966 "compiler.tab.c"
    break;

  case 114: /* ExprValue: IDENT '(' FunctionParameterStmtList ')'  */
#line 312 "compiler.y"
                                              { print_ident((yyvsp[-3].s_var),-1); printf("call: %s%s\n",(yyvsp[-3].s_var),find_call(syT,(yyvsp[-3].s_var))); }
#line 1972 "compiler.tab.c"
    break;

  case 115: /* ExprValue: '(' Expr ')'  */
#line 313 "compiler.y"
                   { pushFunInParm(&(yyvsp[-2].object_var)); }
#line 1978 "compiler.tab.c"
    break;

  case 117: /* ExprLeftSide: IDENT  */
#line 318 "compiler.y"
            { print_ident((yyvsp[0].s_var),find_address(syT,(yyvsp[0].s_var))); cur_value_type = find_ident_type(syT,(yyvsp[0].s_var)); }
#line 1984 "compiler.tab.c"
    break;

  case 118: /* ExprLeftSide: IDENT ArrayStmt  */
#line 319 "compiler.y"
                      { print_ident((yyvsp[-1].s_var),find_address(syT,(yyvsp[-1].s_var))); cur_value_type = find_ident_type(syT,(yyvsp[-1].s_var)); }
#line 1990 "compiler.tab.c"
    break;


#line 1994 "compiler.tab.c"

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

#line 322 "compiler.y"

/* C code section */
SymbolT* crtSymbolT(int insertScope){
    printf("> Create symbol table (scope level %d)\n",insertScope);

    if(syT != NULL){
        return syT;
    }
    else{
        int size = sizeof(SymbolT);
        return (SymbolT*)malloc(size);
    }
}

void insSymbolT(SymbolT* syT,int type,char* name,int addr,int lineno,char* func_sig,int currentScope){
    printf("> Insert `%s` (addr: %d) to scope level %d\n",name,addr,currentScope);
    
    int size = sizeof(SymbolTColumn);
    SymbolTColumn* column = (SymbolTColumn*)malloc(size);
    column->scope_level = currentScope;
    column->index = find_index(syT,currentScope);
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

    if(tailColumn != NULL) {
        tailColumn->next = column;
        tailColumn = column;
    }
    else{
        syT->head = column;
        tailColumn = column;
    }
}

void printSymbolTable(SymbolTColumn* currentColumn, int currentScope){
    printf("\n");
    printf("> Dump symbol table (scope level: %d)\n",currentScope);
    printf("%-10s%-20s%-10s%-10s%-10s%-10s\n","Index","Name","Type","Addr","Lineno","Func_sig");
    
    while(currentColumn != NULL){
        if ( currentColumn->scope_level == currentScope && currentColumn->print == 0 ) {
            printf("%-10d%-20s%-10s%-10d%-10d%-10s\n", currentColumn->index, currentColumn->name, change_num_to_type(currentColumn->type), currentColumn->addr, currentColumn->lineno, currentColumn->func_sig);
            currentColumn->print = 1;
        }
        currentColumn = currentColumn->next;
    }
}

void print_ident(char* name,int address){
    printf("IDENT (name=%s, address=%d)\n",name,address);
}

void print_cout(){
    printf("cout"); 
    for(int i=0; i<cout_counter ; i++){
        printf(" %s",change_num_to_type(stack_cout[i]));
    }
    printf("\n");
    cout_counter = 0;
}

char* change_num_to_type(int currentType){
    if( currentType == 0 )         return "function";
    else if( currentType == 1 )    return "auto";
    else if( currentType == 2 )    return "void";
    else if( currentType == 3 )    return "char";
    else if( currentType == 4 )    return "int";
    else if( currentType == 5 )    return "long";
    else if( currentType == 6 )    return "float";
    else if( currentType == 7 )    return "double";
    else if( currentType == 8 )    return "bool";
    else if( currentType == 9 )    return "string";
    else                           return change_num_to_type(stack_type[currentType]);
}

void find_func_sig(SymbolT* syT,char* id){
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

char* find_call(SymbolT* syT,char* id){
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( strcmp(currentColumn->name,id) == 0 ){
            return currentColumn->func_sig;
        }
        currentColumn = currentColumn->next;
    }
    return "-";
}

int find_index(SymbolT* syT,int currentScope){
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

int find_ident_type(SymbolT* syT,char* id){
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( strcmp(currentColumn->name,id) == 0 ){
            return currentColumn -> type;
        }
        currentColumn = currentColumn->next;
    }
    return 9;
}

int find_address(SymbolT* syT,char* id){
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

void construct_func(){
    strcpy(tempFuncSig[tempFuncSig_counter],"(");
    for(int i=0 ; i < func_sig_counter ; i++){
        if(func_sig_array[i] > 10){
            strcat(tempFuncSig[tempFuncSig_counter],"[");
            func_sig_array[i] -= 10;
        }

        if(func_sig_array[i] == 2)         strcat(tempFuncSig[tempFuncSig_counter],"V");
        else if(func_sig_array[i] == 4)    strcat(tempFuncSig[tempFuncSig_counter],"I");
        else if(func_sig_array[i] == 8)    strcat(tempFuncSig[tempFuncSig_counter],"B");
        else if(func_sig_array[i] == 9)    strcat(tempFuncSig[tempFuncSig_counter],"Ljava/lang/String;");

        if(i == func_sig_counter-2)        strcat(tempFuncSig[tempFuncSig_counter],")");
    }
    tempFuncSig_counter++;
    func_sig_counter = 0;
}
