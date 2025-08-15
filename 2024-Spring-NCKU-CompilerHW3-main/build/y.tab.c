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
#line 2 "./compiler.y"

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
    int change_line = 0;
    int cur_value_type = 1;
    int store_addr = 0;
    int cur_addr = 0;
    int lineType = 0;
    int loopNum = 0;
    int stack_type[30];
    int stack_if[30];
    int stack_if_counter = 0;
    int while_count = 0;
    int stack_for[30];
    int stack_for_counter = 0;
    int array_counter = 0;
    int array_mode = 0;
    int array_dim = 0;
    int func_if = 0;
    int func_sig_array[30];
    int func_sig_counter = 0;
    int tempFuncSig_counter = 0;
    char tempFuncSig[10][80];
    SymbolT* syT = NULL;
    SymbolTColumn* tailColumn = NULL;
    FILE *jasminFile;

    void init_jasminFile();
    void writeFunc_jasminFile();
    void leaveFunc_jasminFile(int type,char*id);
    SymbolT* crt_SymbolT(int insertScope);
    void ins_SymbolT(int type,char* name,int addr,int lineno,char* func_sig,int currentScope);
    char* find_call(char* id);
    char* change_num_to_type(int currentType);
    int find_index(int currentScope);
    int find_func_type(char* sig);
    int find_ident_address(char* id);
    int find_ident_type(char* id);
    int find_ident_type_inAddr(int address);
    void ins_func_sig(char *id);
    void construct_func(char *id);
    void fprint_funcsig();
    void fprintf_type();
    void token_GTRGEQLEQLES(int action);
    void token_EQLNEQNOT(int action);
    void token_INCDEC(int action);
    void token_IFELSE(int action);
    void token_FOR(int action);
    void token_BNT();
    void transformIdentType(int type);

#line 146 "./build/y.tab.c"

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

#include "y.tab.h"
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
  YYSYMBOL_83_11 = 83,                     /* $@11  */
  YYSYMBOL_CoutParmListStmt = 84,          /* CoutParmListStmt  */
  YYSYMBOL_85_12 = 85,                     /* $@12  */
  YYSYMBOL_86_13 = 86,                     /* $@13  */
  YYSYMBOL_IfElseStmt = 87,                /* IfElseStmt  */
  YYSYMBOL_88_14 = 88,                     /* $@14  */
  YYSYMBOL_ForStmt = 89,                   /* ForStmt  */
  YYSYMBOL_90_15 = 90,                     /* $@15  */
  YYSYMBOL_91_16 = 91,                     /* $@16  */
  YYSYMBOL_ForInit = 92,                   /* ForInit  */
  YYSYMBOL_ArrayDefStmt = 93,              /* ArrayDefStmt  */
  YYSYMBOL_ArrayStmt = 94,                 /* ArrayStmt  */
  YYSYMBOL_95_17 = 95,                     /* $@17  */
  YYSYMBOL_ArrayVariable = 96,             /* ArrayVariable  */
  YYSYMBOL_ArrayExpression = 97,           /* ArrayExpression  */
  YYSYMBOL_ArrayInsideExpression = 98,     /* ArrayInsideExpression  */
  YYSYMBOL_99_18 = 99,                     /* $@18  */
  YYSYMBOL_100_19 = 100,                   /* $@19  */
  YYSYMBOL_StmtParameter = 101,            /* StmtParameter  */
  YYSYMBOL_StmtParameterList = 102,        /* StmtParameterList  */
  YYSYMBOL_Expr = 103,                     /* Expr  */
  YYSYMBOL_104_20 = 104,                   /* $@20  */
  YYSYMBOL_105_21 = 105,                   /* $@21  */
  YYSYMBOL_106_22 = 106,                   /* $@22  */
  YYSYMBOL_107_23 = 107,                   /* $@23  */
  YYSYMBOL_108_24 = 108,                   /* $@24  */
  YYSYMBOL_109_25 = 109,                   /* $@25  */
  YYSYMBOL_110_26 = 110,                   /* $@26  */
  YYSYMBOL_111_27 = 111,                   /* $@27  */
  YYSYMBOL_112_28 = 112,                   /* $@28  */
  YYSYMBOL_113_29 = 113,                   /* $@29  */
  YYSYMBOL_114_30 = 114,                   /* $@30  */
  YYSYMBOL_115_31 = 115,                   /* $@31  */
  YYSYMBOL_116_32 = 116,                   /* $@32  */
  YYSYMBOL_117_33 = 117,                   /* $@33  */
  YYSYMBOL_118_34 = 118,                   /* $@34  */
  YYSYMBOL_119_35 = 119,                   /* $@35  */
  YYSYMBOL_120_36 = 120,                   /* $@36  */
  YYSYMBOL_121_37 = 121,                   /* $@37  */
  YYSYMBOL_ExprLeftSide = 122,             /* ExprLeftSide  */
  YYSYMBOL_123_38 = 123,                   /* $@38  */
  YYSYMBOL_ExprRightSide = 124,            /* ExprRightSide  */
  YYSYMBOL_ExprDoubleAnd = 125,            /* ExprDoubleAnd  */
  YYSYMBOL_ExprOr = 126,                   /* ExprOr  */
  YYSYMBOL_ExprXor = 127,                  /* ExprXor  */
  YYSYMBOL_ExprAnd = 128,                  /* ExprAnd  */
  YYSYMBOL_ExprSame = 129,                 /* ExprSame  */
  YYSYMBOL_ExprBigOrSmall = 130,           /* ExprBigOrSmall  */
  YYSYMBOL_ExprBitwise = 131,              /* ExprBitwise  */
  YYSYMBOL_ExprPlus = 132,                 /* ExprPlus  */
  YYSYMBOL_ExprCross = 133,                /* ExprCross  */
  YYSYMBOL_ExprFrontUnary = 134,           /* ExprFrontUnary  */
  YYSYMBOL_135_39 = 135,                   /* $@39  */
  YYSYMBOL_ExprPostfix = 136,              /* ExprPostfix  */
  YYSYMBOL_ExprValue = 137,                /* ExprValue  */
  YYSYMBOL_138_40 = 138                    /* $@40  */
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
typedef yytype_int16 yy_state_t;

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
#define YYLAST   408

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  61
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  78
/* YYNRULES -- Number of rules.  */
#define YYNRULES  155
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  257

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
       0,   137,   137,   137,   138,   142,   143,   147,   148,   152,
     157,   157,   157,   157,   161,   162,   163,   167,   168,   169,
     174,   175,   179,   180,   181,   182,   182,   182,   183,   183,
     183,   183,   184,   184,   185,   186,   187,   188,   192,   193,
     197,   198,   199,   199,   200,   204,   204,   205,   205,   209,
     210,   210,   214,   214,   214,   215,   220,   221,   225,   226,
     230,   230,   231,   235,   236,   240,   244,   244,   245,   245,
     246,   250,   251,   255,   256,   260,   261,   261,   261,   262,
     262,   262,   263,   263,   263,   264,   264,   264,   265,   265,
     265,   266,   266,   266,   267,   267,   267,   268,   268,   268,
     269,   269,   269,   270,   274,   275,   275,   276,   280,   281,
     285,   286,   290,   291,   295,   296,   300,   301,   305,   306,
     307,   311,   312,   313,   314,   315,   319,   320,   321,   325,
     326,   327,   331,   332,   333,   334,   338,   339,   340,   341,
     342,   342,   343,   347,   348,   349,   353,   354,   355,   356,
     357,   358,   359,   360,   360,   361
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
  "$@9", "$@10", "Elsestmt", "RegularDefineStmt", "$@11",
  "CoutParmListStmt", "$@12", "$@13", "IfElseStmt", "$@14", "ForStmt",
  "$@15", "$@16", "ForInit", "ArrayDefStmt", "ArrayStmt", "$@17",
  "ArrayVariable", "ArrayExpression", "ArrayInsideExpression", "$@18",
  "$@19", "StmtParameter", "StmtParameterList", "Expr", "$@20", "$@21",
  "$@22", "$@23", "$@24", "$@25", "$@26", "$@27", "$@28", "$@29", "$@30",
  "$@31", "$@32", "$@33", "$@34", "$@35", "$@36", "$@37", "ExprLeftSide",
  "$@38", "ExprRightSide", "ExprDoubleAnd", "ExprOr", "ExprXor", "ExprAnd",
  "ExprSame", "ExprBigOrSmall", "ExprBitwise", "ExprPlus", "ExprCross",
  "ExprFrontUnary", "$@39", "ExprPostfix", "ExprValue", "$@40", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-203)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-154)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      19,    24,   -11,  -203,    15,   -11,  -203,  -203,  -203,    50,
    -203,   335,    27,   335,   335,   335,   335,  -203,  -203,  -203,
    -203,  -203,   -47,     5,    -5,    64,    81,    84,    92,    36,
      10,    66,    73,    83,  -203,  -203,    56,  -203,  -203,  -203,
    -203,  -203,   271,    59,    76,    11,     5,    82,   334,   114,
     335,  -203,   335,   335,   335,   335,   335,   335,   335,   335,
     335,   335,   335,   335,   335,   335,   335,   335,   335,  -203,
    -203,   271,    88,   -40,  -203,    73,   -16,    85,  -203,    59,
      86,  -203,   344,   116,   119,   122,   124,   121,   130,   132,
     131,   133,    64,    81,    84,    92,    36,    10,    10,    66,
      66,    66,    66,    73,    73,    83,    83,  -203,  -203,  -203,
       9,   107,  -203,   271,  -203,  -203,   110,   108,   109,    85,
     374,  -203,   344,   344,   344,   344,   344,   344,   344,   344,
     344,  -203,   111,  -203,  -203,   -16,   344,  -203,  -203,  -203,
    -203,  -203,  -203,  -203,  -203,  -203,  -203,   113,  -203,   115,
    -203,  -203,  -203,  -203,  -203,  -203,  -203,  -203,  -203,   261,
    -203,   167,   137,  -203,  -203,   280,   139,   142,   146,  -203,
      79,  -203,   145,  -203,  -203,   147,   344,   148,   151,  -203,
     154,  -203,  -203,   -12,   152,  -203,  -203,  -203,  -203,   335,
    -203,   203,   156,   326,   344,  -203,   335,   -16,   187,   157,
     164,    73,   335,  -203,   159,   162,  -203,  -203,   114,   158,
     198,   -16,   199,  -203,    73,   261,   344,   169,   174,   173,
     172,   188,   202,  -203,  -203,   191,  -203,   261,   344,  -203,
      53,  -203,  -203,   227,   261,   136,   215,   213,    55,   335,
     261,  -203,   192,  -203,  -203,   261,  -203,  -203,    73,  -203,
    -203,   344,   210,   335,  -203,  -203,    73
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     0,     1,     0,     3,     6,     7,     8,    10,
       5,     0,     0,     0,     0,     0,     0,   148,   149,   146,
     147,   150,   152,     0,     0,   109,   111,   113,   115,   117,
     120,   125,   128,   131,   135,   142,   145,    11,   137,   138,
     139,   136,    16,     0,     0,   152,     0,     0,    76,   103,
       0,     9,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   143,
     144,    16,     0,     0,    15,    19,     0,   154,   140,     0,
       0,   155,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   108,   110,   112,   114,   116,   118,   119,   121,
     124,   122,   123,   126,   127,   129,   130,   132,   133,   134,
       0,    17,   151,     0,    64,    63,     0,     0,     0,   106,
     155,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    12,     0,    14,    62,     0,     0,   141,    77,    80,
      83,    86,    89,    92,    95,    98,   101,     0,    18,     0,
      78,    81,    84,    87,    90,    93,    96,    99,   102,     0,
      61,     0,     0,    32,    28,     0,     0,     0,     0,    22,
       0,    21,     0,    44,    47,    45,     0,     0,     0,    36,
       0,    34,    35,    74,    40,    72,    13,    20,    23,     0,
      24,     0,     0,    57,     0,    37,     0,     0,    41,     0,
       0,    48,     0,    25,    56,     0,    52,    29,    73,     0,
       0,     0,    74,    71,    46,    50,     0,     0,     0,     0,
      59,     0,     0,    49,    26,     0,    55,     0,     0,    30,
      68,    43,    58,    39,     0,     0,     0,     0,     0,     0,
      50,    27,     0,    33,    53,     0,    65,    66,    69,    38,
      51,     0,     0,     0,    54,    31,    67
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -203,  -203,  -203,  -203,   265,  -203,  -203,  -203,  -203,  -203,
     204,   160,  -202,  -167,  -203,  -203,  -203,  -203,  -203,  -203,
    -203,    87,  -203,  -203,  -203,  -203,    34,  -203,  -203,  -203,
    -203,  -203,  -203,   -75,  -203,  -130,  -203,  -203,  -203,  -203,
    -203,    77,   -23,  -203,  -203,  -203,  -203,  -203,  -203,  -203,
    -203,  -203,  -203,  -203,  -203,  -203,  -203,  -203,  -203,  -203,
    -203,  -203,  -203,    -9,   228,   231,   232,   230,   234,    58,
      18,   -41,    69,    -6,  -203,   175,  -203,  -203
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     1,     2,     5,     6,     7,     8,    12,    71,   147,
      73,    74,   170,   171,   215,   233,   178,   219,   237,   177,
     241,   172,   210,   175,   191,   189,   224,   225,   205,   218,
     251,   206,   198,    77,   117,   116,   231,   238,   253,   239,
     184,   185,   173,    83,   150,    84,   151,    85,   152,    86,
     153,    87,   154,    88,   155,    89,   156,    90,   157,    91,
     158,    48,    79,    49,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,   118,    35,    36,    43
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      47,    75,    24,   187,   119,   149,    42,    38,    39,    40,
      41,  -153,   196,    13,   112,    14,    15,   113,    50,    -4,
      16,   103,   104,    80,     3,   235,    58,    59,    60,    61,
      75,   114,   242,     4,   115,  -104,  -104,  -104,  -104,  -104,
    -104,  -104,  -104,   252,  -104,  -104,   197,    51,   223,    44,
      17,    18,    19,    20,    21,    45,    56,    57,    46,   121,
     107,   108,   109,   131,    42,     9,   113,   209,   187,  -105,
      62,    63,    75,   223,    11,   187,    99,   100,   101,   102,
      37,   222,   161,    64,    65,   187,    52,    13,    53,    14,
      15,    69,    70,    54,    16,    66,    67,    68,    55,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   199,   -70,
     -70,   246,   247,    47,    97,    98,   162,    76,   163,   164,
     165,   166,   167,   168,    17,    18,    19,    20,    21,    45,
      78,   169,    46,   105,   106,   186,    81,    50,   111,   161,
     120,   122,   180,   -60,    13,   123,    14,    15,   201,   124,
     126,    16,   125,   192,    17,    18,    19,    20,    21,    22,
     127,   214,   136,   128,   129,   132,   135,   130,   159,   134,
     148,   207,   174,   162,   160,   163,   164,   165,   166,   167,
     168,    17,    18,    19,    20,    21,    45,   208,   169,    46,
     176,   181,   243,   226,   182,   161,   183,   188,   248,   190,
      13,   193,    14,    15,   194,   236,   195,    16,   202,   200,
     203,   -42,   256,   161,   212,   211,   217,   220,    13,   216,
      14,    15,   221,   196,   227,    16,   228,   229,   254,   162,
     -62,   163,   164,   165,   166,   167,   168,    17,    18,    19,
      20,    21,    45,   230,   169,    46,   234,   162,   250,   163,
     164,   165,   166,   167,   168,    17,    18,    19,    20,    21,
      45,   232,   169,    46,   161,   240,   255,   244,   245,    13,
      10,    14,    15,   133,   249,   110,    16,   213,    92,    13,
     204,    14,    15,    93,    95,    94,    16,     0,    13,    96,
      14,    15,     0,   137,     0,    16,     0,     0,   162,     0,
     163,   164,   165,   166,   167,   168,    17,    18,    19,    20,
      21,    45,     0,   169,    46,    72,    17,    18,    19,    20,
      21,    22,     0,     0,    23,    17,    18,    19,    20,    21,
      45,     0,   179,    46,    13,     0,    14,    15,     0,     0,
       0,    16,     0,    13,     0,    14,    15,     0,     0,     0,
      16,     0,    13,     0,    14,    15,     0,     0,    82,    16,
     -79,   -82,   -85,   -88,   -91,   -94,     0,   -97,  -100,     0,
     168,    17,    18,    19,    20,    21,    45,     0,     0,    46,
      17,    18,    19,    20,    21,    22,     0,     0,    23,    17,
      18,    19,    20,    21,    45,     0,     0,    46,  -107,  -107,
    -107,  -107,  -107,  -107,  -107,  -107,     0,  -107,  -107
};

static const yytype_int16 yycheck[] =
{
      23,    42,    11,   170,    79,   135,    53,    13,    14,    15,
      16,    58,    24,     8,    54,    10,    11,    57,    23,     0,
      15,    62,    63,    46,     0,   227,    16,    17,    18,    19,
      71,    47,   234,    44,    50,    24,    25,    26,    27,    28,
      29,    30,    31,   245,    33,    34,    58,    52,   215,    44,
      45,    46,    47,    48,    49,    50,    20,    21,    53,    82,
      66,    67,    68,    54,    53,    50,    57,   197,   235,    58,
       4,     5,   113,   240,    24,   242,    58,    59,    60,    61,
      53,   211,     3,    10,    11,   252,    22,     8,     7,    10,
      11,    35,    36,     9,    15,    12,    13,    14,     6,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   183,    56,
      57,    56,    57,   136,    56,    57,    37,    58,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      54,    52,    53,    64,    65,    56,    54,    23,    50,     3,
      54,    25,   165,    58,     8,    26,    10,    11,   189,    27,
      29,    15,    28,   176,    45,    46,    47,    48,    49,    50,
      30,   202,    53,    31,    33,    58,    58,    34,    55,    59,
      59,   194,     5,    37,    59,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,   196,    52,    53,
      53,    52,    56,   216,    52,     3,    50,    52,   239,    52,
       8,    53,    10,    11,    53,   228,    52,    15,     5,    57,
      54,    24,   253,     3,    50,    58,    54,    59,     8,    60,
      10,    11,    24,    24,    55,    15,    52,    54,   251,    37,
      58,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    55,    52,    53,    55,    37,    56,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    59,    52,    53,     3,    38,    56,    52,    55,     8,
       5,    10,    11,   113,   240,    71,    15,   200,    50,     8,
     193,    10,    11,    52,    54,    53,    15,    -1,     8,    55,
      10,    11,    -1,   118,    -1,    15,    -1,    -1,    37,    -1,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    -1,    52,    53,    44,    45,    46,    47,    48,
      49,    50,    -1,    -1,    53,    45,    46,    47,    48,    49,
      50,    -1,    52,    53,     8,    -1,    10,    11,    -1,    -1,
      -1,    15,    -1,     8,    -1,    10,    11,    -1,    -1,    -1,
      15,    -1,     8,    -1,    10,    11,    -1,    -1,    24,    15,
      26,    27,    28,    29,    30,    31,    -1,    33,    34,    -1,
      44,    45,    46,    47,    48,    49,    50,    -1,    -1,    53,
      45,    46,    47,    48,    49,    50,    -1,    -1,    53,    45,
      46,    47,    48,    49,    50,    -1,    -1,    53,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    33,    34
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    62,    63,     0,    44,    64,    65,    66,    67,    50,
      65,    24,    68,     8,    10,    11,    15,    45,    46,    47,
      48,    49,    50,    53,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,   134,   136,   137,    53,   134,   134,
     134,   134,    53,   138,    44,    50,    53,   103,   122,   124,
      23,    52,    22,     7,     9,     6,    20,    21,    16,    17,
      18,    19,     4,     5,    10,    11,    12,    13,    14,    35,
      36,    69,    44,    71,    72,   132,    58,    94,    54,   123,
     103,    54,    24,   104,   106,   108,   110,   112,   114,   116,
     118,   120,   125,   126,   127,   128,   129,   130,   130,   131,
     131,   131,   131,   132,   132,   133,   133,   134,   134,   134,
      71,    50,    54,    57,    47,    50,    96,    95,   135,    94,
      54,   103,    25,    26,    27,    28,    29,    30,    31,    33,
      34,    54,    58,    72,    59,    58,    53,   136,   103,   103,
     103,   103,   103,   103,   103,   103,   103,    70,    59,    96,
     105,   107,   109,   111,   113,   115,   117,   119,   121,    55,
      59,     3,    37,    39,    40,    41,    42,    43,    44,    52,
      73,    74,    82,   103,     5,    84,    53,    80,    77,    52,
     103,    52,    52,    50,   101,   102,    56,    74,    52,    86,
      52,    85,   103,    53,    53,    52,    24,    58,    93,    94,
      57,   132,     5,    54,    82,    89,    92,   103,   124,    96,
      83,    58,    50,   102,   132,    75,    60,    54,    90,    78,
      59,    24,    96,    74,    87,    88,   103,    55,    52,    54,
      55,    97,    59,    76,    55,    73,   103,    79,    98,   100,
      38,    81,    73,    56,    52,    55,    56,    57,   132,    87,
      56,    91,    73,    99,   103,    56,   132
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,    61,    63,    62,    62,    64,    64,    65,    65,    66,
      68,    69,    70,    67,    71,    71,    71,    72,    72,    72,
      73,    73,    74,    74,    74,    75,    76,    74,    77,    78,
      79,    74,    80,    74,    74,    74,    74,    74,    81,    81,
      82,    82,    83,    82,    82,    85,    84,    86,    84,    87,
      88,    87,    90,    91,    89,    89,    92,    92,    93,    93,
      95,    94,    94,    96,    96,    97,    99,    98,   100,    98,
      98,   101,   101,   102,   102,   103,   104,   105,   103,   106,
     107,   103,   108,   109,   103,   110,   111,   103,   112,   113,
     103,   114,   115,   103,   116,   117,   103,   118,   119,   103,
     120,   121,   103,   103,   122,   123,   122,   122,   124,   124,
     125,   125,   126,   126,   127,   127,   128,   128,   129,   129,
     129,   130,   130,   130,   130,   130,   131,   131,   131,   132,
     132,   132,   133,   133,   133,   133,   134,   134,   134,   134,
     135,   134,   134,   136,   136,   136,   137,   137,   137,   137,
     137,   137,   137,   138,   137,   137
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     0,     2,     1,     1,     1,     5,
       0,     0,     0,    11,     3,     1,     0,     2,     4,     1,
       2,     1,     1,     2,     3,     0,     0,     8,     0,     0,
       0,    10,     0,     8,     2,     2,     2,     3,     2,     0,
       2,     3,     0,     6,     1,     0,     4,     0,     3,     1,
       0,     4,     0,     0,     7,     3,     1,     0,     4,     3,
       0,     5,     3,     1,     1,     3,     0,     4,     0,     2,
       0,     3,     1,     3,     1,     3,     0,     0,     5,     0,
       0,     5,     0,     0,     5,     0,     0,     5,     0,     0,
       5,     0,     0,     5,     0,     0,     5,     0,     0,     5,
       0,     0,     5,     1,     1,     0,     3,     3,     3,     1,
       3,     1,     3,     1,     3,     1,     3,     1,     3,     3,
       1,     3,     3,     3,     3,     1,     3,     3,     1,     3,
       3,     1,     3,     3,     3,     1,     2,     2,     2,     2,
       0,     5,     1,     2,     2,     1,     1,     1,     1,     1,
       1,     4,     1,     0,     3,     3
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
#line 137 "./compiler.y"
      { init_jasminFile(); syT=crt_SymbolT(++scope); }
#line 1496 "./build/y.tab.c"
    break;

  case 3: /* Program: $@1 GlobalStmtList  */
#line 137 "./compiler.y"
                                                                      { scope--; fclose(jasminFile); }
#line 1502 "./build/y.tab.c"
    break;

  case 10: /* $@2: %empty  */
#line 157 "./compiler.y"
                       { address=0; if(strcmp((yyvsp[0].s_var),"mod") == 0){func_if=1;} ins_SymbolT(0,(yyvsp[0].s_var),-1,yylineno,"unknown",scope); }
#line 1508 "./build/y.tab.c"
    break;

  case 11: /* $@3: %empty  */
#line 157 "./compiler.y"
                                                                                                                                             { syT=crt_SymbolT(++scope); }
#line 1514 "./build/y.tab.c"
    break;

  case 12: /* $@4: %empty  */
#line 157 "./compiler.y"
                                                                                                                                                                                                         { func_sig_array[func_sig_counter++]=(yyvsp[-6].var_type); construct_func((yyvsp[-5].s_var)); ins_func_sig((yyvsp[-5].s_var)); writeFunc_jasminFile((yyvsp[-5].s_var),find_call((yyvsp[-5].s_var))); }
#line 1520 "./build/y.tab.c"
    break;

  case 13: /* FunctionDefStmt: VARIABLE_T IDENT $@2 '(' $@3 FunctionParameterStmtList ')' $@4 '{' StmtList '}'  */
#line 157 "./compiler.y"
                                                                                                                                                                                                                                                                                                                                                                                         {leaveFunc_jasminFile((yyvsp[-10].var_type),(yyvsp[-9].s_var)); scope--;}
#line 1526 "./build/y.tab.c"
    break;

  case 17: /* FunctionParameterStmt: VARIABLE_T IDENT  */
#line 167 "./compiler.y"
                       { ins_SymbolT((yyvsp[-1].var_type),(yyvsp[0].s_var),address++,yylineno,"-",scope);  func_sig_array[func_sig_counter++]=(yyvsp[-1].var_type); }
#line 1532 "./build/y.tab.c"
    break;

  case 18: /* FunctionParameterStmt: VARIABLE_T IDENT '[' ']'  */
#line 168 "./compiler.y"
                               { ins_SymbolT((yyvsp[-3].var_type),(yyvsp[-2].s_var),address++,yylineno,"-",scope); func_sig_array[func_sig_counter++]=((yyvsp[-3].var_type)+12); }
#line 1538 "./build/y.tab.c"
    break;

  case 25: /* $@5: %empty  */
#line 182 "./compiler.y"
                      { token_IFELSE(1); }
#line 1544 "./build/y.tab.c"
    break;

  case 26: /* $@6: %empty  */
#line 182 "./compiler.y"
                                                      { token_IFELSE(2); }
#line 1550 "./build/y.tab.c"
    break;

  case 27: /* Stmt: IF '(' Expr ')' $@5 IfElseStmt $@6 Elsestmt  */
#line 182 "./compiler.y"
                                                                                    { token_IFELSE(3); }
#line 1556 "./build/y.tab.c"
    break;

  case 28: /* $@7: %empty  */
#line 183 "./compiler.y"
            { fprintf(jasminFile, "W_alpha_%d:\n",++while_count); }
#line 1562 "./build/y.tab.c"
    break;

  case 29: /* $@8: %empty  */
#line 183 "./compiler.y"
                                                                             { syT=crt_SymbolT(++scope); }
#line 1568 "./build/y.tab.c"
    break;

  case 30: /* $@9: %empty  */
#line 183 "./compiler.y"
                                                                                                               { fprintf(jasminFile, "\tifeq W_beta_%d\n",while_count); }
#line 1574 "./build/y.tab.c"
    break;

  case 31: /* Stmt: WHILE $@7 '(' Expr $@8 ')' $@9 '{' StmtList '}'  */
#line 183 "./compiler.y"
                                                                                                                                                                                           { fprintf(jasminFile, "\tgoto W_alpha_%d\n",while_count); fprintf(jasminFile, "W_beta_%d:\n",while_count); }
#line 1580 "./build/y.tab.c"
    break;

  case 32: /* $@10: %empty  */
#line 184 "./compiler.y"
          { syT=crt_SymbolT(++scope); stack_for[++stack_for_counter]=loopNum;}
#line 1586 "./build/y.tab.c"
    break;

  case 33: /* Stmt: FOR $@10 '(' ForStmt ')' '{' StmtList '}'  */
#line 184 "./compiler.y"
                                                                                                                { fprintf(jasminFile, "goto f_beta_%d\n",stack_for[stack_for_counter]); fprintf(jasminFile, "f_Delta_%d:\n",stack_for[stack_for_counter--]); }
#line 1592 "./build/y.tab.c"
    break;

  case 34: /* Stmt: BREAK ';'  */
#line 185 "./compiler.y"
                { fprintf(jasminFile, "\tgoto W_beta_%d\n",while_count); }
#line 1598 "./build/y.tab.c"
    break;

  case 40: /* RegularDefineStmt: VARIABLE_T StmtParameter  */
#line 197 "./compiler.y"
                               { if((yyvsp[-1].var_type)!=1){ cur_value_type=(yyvsp[-1].var_type); } stack_type[type_no++]=cur_value_type; }
#line 1604 "./build/y.tab.c"
    break;

  case 41: /* RegularDefineStmt: VARIABLE_T IDENT ArrayDefStmt  */
#line 198 "./compiler.y"
                                    { array_counter=0; if(array_dim==0) { fprintf(jasminFile, "newarray %s\n",change_num_to_type((yyvsp[-2].var_type))); } else {fprintf(jasminFile, "multianewarray [[I %d\n",array_dim); array_dim=0;} ins_SymbolT((yyvsp[-2].var_type),(yyvsp[-1].s_var),address++,yylineno,"-",scope); fprintf(jasminFile, "\tastore %d\n",find_ident_address((yyvsp[-1].s_var)));}
#line 1610 "./build/y.tab.c"
    break;

  case 42: /* $@11: %empty  */
#line 199 "./compiler.y"
                                    { array_counter=0; if(array_dim==0) { fprintf(jasminFile, "newarray %s\n",change_num_to_type((yyvsp[-2].var_type))); } else {fprintf(jasminFile, "multianewarray [[I %d\n",array_dim); array_dim=0;} }
#line 1616 "./build/y.tab.c"
    break;

  case 43: /* RegularDefineStmt: VARIABLE_T IDENT ArrayDefStmt $@11 VAL_ASSIGN ArrayExpression  */
#line 199 "./compiler.y"
                                                                                                                                                                                                                                                             { ins_SymbolT((yyvsp[-5].var_type),(yyvsp[-4].s_var),address++,yylineno,"-",scope); fprintf(jasminFile, "\tastore %d\n",find_ident_address((yyvsp[-4].s_var))); }
#line 1622 "./build/y.tab.c"
    break;

  case 45: /* $@12: %empty  */
#line 204 "./compiler.y"
                       {fprintf(jasminFile, "\n\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");}
#line 1628 "./build/y.tab.c"
    break;

  case 46: /* CoutParmListStmt: CoutParmListStmt $@12 SHL ExprPlus  */
#line 204 "./compiler.y"
                                                                                                                          {fprint_funcsig();}
#line 1634 "./build/y.tab.c"
    break;

  case 47: /* $@13: %empty  */
#line 205 "./compiler.y"
          {fprintf(jasminFile, "\n\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");}
#line 1640 "./build/y.tab.c"
    break;

  case 48: /* CoutParmListStmt: SHL $@13 ExprPlus  */
#line 205 "./compiler.y"
                                                                                                        {fprint_funcsig();}
#line 1646 "./build/y.tab.c"
    break;

  case 50: /* $@14: %empty  */
#line 210 "./compiler.y"
      { syT=crt_SymbolT(++scope); }
#line 1652 "./build/y.tab.c"
    break;

  case 51: /* IfElseStmt: $@14 '{' StmtList '}'  */
#line 210 "./compiler.y"
                                                     { scope--; }
#line 1658 "./build/y.tab.c"
    break;

  case 52: /* $@15: %empty  */
#line 214 "./compiler.y"
              { token_FOR(1); }
#line 1664 "./build/y.tab.c"
    break;

  case 53: /* $@16: %empty  */
#line 214 "./compiler.y"
                                             { token_FOR(2); }
#line 1670 "./build/y.tab.c"
    break;

  case 54: /* ForStmt: ForInit $@15 ';' Expr ';' $@16 Expr  */
#line 214 "./compiler.y"
                                                                    { token_FOR(3); }
#line 1676 "./build/y.tab.c"
    break;

  case 58: /* ArrayDefStmt: ArrayStmt '[' ArrayVariable ']'  */
#line 225 "./compiler.y"
                                      {array_dim = 2;}
#line 1682 "./build/y.tab.c"
    break;

  case 60: /* $@17: %empty  */
#line 230 "./compiler.y"
                { fprintf(jasminFile, "\taaload\n"); }
#line 1688 "./build/y.tab.c"
    break;

  case 63: /* ArrayVariable: IDENT  */
#line 235 "./compiler.y"
            { if(strcmp((yyvsp[0].s_var),"endl")==0){ cur_value_type=11; change_line=1; } else{ cur_value_type = find_ident_type((yyvsp[0].s_var)); cur_addr=find_ident_address((yyvsp[0].s_var)); fprintf_type(); fprintf(jasminFile, "load %d\n",cur_addr); }}
#line 1694 "./build/y.tab.c"
    break;

  case 64: /* ArrayVariable: INT_LIT  */
#line 236 "./compiler.y"
              { fprintf(jasminFile, "\tldc %d\n",(yyvsp[0].i_var)); }
#line 1700 "./build/y.tab.c"
    break;

  case 66: /* $@18: %empty  */
#line 244 "./compiler.y"
                               {fprintf(jasminFile, "\tdup\n");fprintf(jasminFile, "\tldc %d\n",array_counter++);}
#line 1706 "./build/y.tab.c"
    break;

  case 67: /* ArrayInsideExpression: ArrayInsideExpression ',' $@18 ExprPlus  */
#line 244 "./compiler.y"
                                                                                                                            { fprintf_type(); fprintf(jasminFile, "astore\n"); }
#line 1712 "./build/y.tab.c"
    break;

  case 68: /* $@19: %empty  */
#line 245 "./compiler.y"
      {fprintf(jasminFile, "\tdup\n"); fprintf(jasminFile, "\tldc %d\n",array_counter++);}
#line 1718 "./build/y.tab.c"
    break;

  case 69: /* ArrayInsideExpression: $@19 ExprPlus  */
#line 245 "./compiler.y"
                                                                                                    { fprintf_type(); fprintf(jasminFile, "astore\n"); }
#line 1724 "./build/y.tab.c"
    break;

  case 73: /* StmtParameterList: IDENT VAL_ASSIGN ExprRightSide  */
#line 255 "./compiler.y"
                                     {fprintf_type(); fprintf(jasminFile, "store %d\n",address); ins_SymbolT(type_no,(yyvsp[-2].s_var),address++,yylineno,"-",scope); }
#line 1730 "./build/y.tab.c"
    break;

  case 74: /* StmtParameterList: IDENT  */
#line 256 "./compiler.y"
            { ins_SymbolT(type_no,(yyvsp[0].s_var),address++,yylineno,"-",scope); }
#line 1736 "./build/y.tab.c"
    break;

  case 75: /* Expr: ExprLeftSide VAL_ASSIGN Expr  */
#line 260 "./compiler.y"
                                   { if(cur_value_type!=find_ident_type_inAddr(store_addr)) { transformIdentType(find_ident_type_inAddr(store_addr)); } fprintf_type(); if(array_mode==1){ fprintf(jasminFile, "astore\n"); array_mode=0;} else { fprintf(jasminFile, "store %d\n",store_addr); } }
#line 1742 "./build/y.tab.c"
    break;

  case 76: /* $@20: %empty  */
#line 261 "./compiler.y"
                   { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); }
#line 1748 "./build/y.tab.c"
    break;

  case 77: /* $@21: %empty  */
#line 261 "./compiler.y"
                                                                                                    { fprintf_type(); fprintf(jasminFile, "add\n"); }
#line 1754 "./build/y.tab.c"
    break;

  case 78: /* Expr: ExprLeftSide $@20 ADD_ASSIGN Expr $@21  */
#line 261 "./compiler.y"
                                                                                                                                                      { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
#line 1760 "./build/y.tab.c"
    break;

  case 79: /* $@22: %empty  */
#line 262 "./compiler.y"
                   { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); }
#line 1766 "./build/y.tab.c"
    break;

  case 80: /* $@23: %empty  */
#line 262 "./compiler.y"
                                                                                                    { fprintf_type(); fprintf(jasminFile, "sub\n"); }
#line 1772 "./build/y.tab.c"
    break;

  case 81: /* Expr: ExprLeftSide $@22 SUB_ASSIGN Expr $@23  */
#line 262 "./compiler.y"
                                                                                                                                                      { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
#line 1778 "./build/y.tab.c"
    break;

  case 82: /* $@24: %empty  */
#line 263 "./compiler.y"
                   { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); }
#line 1784 "./build/y.tab.c"
    break;

  case 83: /* $@25: %empty  */
#line 263 "./compiler.y"
                                                                                                    { fprintf_type(); fprintf(jasminFile, "mul\n"); }
#line 1790 "./build/y.tab.c"
    break;

  case 84: /* Expr: ExprLeftSide $@24 MUL_ASSIGN Expr $@25  */
#line 263 "./compiler.y"
                                                                                                                                                      { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
#line 1796 "./build/y.tab.c"
    break;

  case 85: /* $@26: %empty  */
#line 264 "./compiler.y"
                   { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); }
#line 1802 "./build/y.tab.c"
    break;

  case 86: /* $@27: %empty  */
#line 264 "./compiler.y"
                                                                                                    { fprintf_type(); fprintf(jasminFile, "div\n"); }
#line 1808 "./build/y.tab.c"
    break;

  case 87: /* Expr: ExprLeftSide $@26 DIV_ASSIGN Expr $@27  */
#line 264 "./compiler.y"
                                                                                                                                                      { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
#line 1814 "./build/y.tab.c"
    break;

  case 88: /* $@28: %empty  */
#line 265 "./compiler.y"
                   { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); }
#line 1820 "./build/y.tab.c"
    break;

  case 89: /* $@29: %empty  */
#line 265 "./compiler.y"
                                                                                                    { fprintf_type(); fprintf(jasminFile, "rem\n"); }
#line 1826 "./build/y.tab.c"
    break;

  case 90: /* Expr: ExprLeftSide $@28 REM_ASSIGN Expr $@29  */
#line 265 "./compiler.y"
                                                                                                                                                      { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
#line 1832 "./build/y.tab.c"
    break;

  case 91: /* $@30: %empty  */
#line 266 "./compiler.y"
                   { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); }
#line 1838 "./build/y.tab.c"
    break;

  case 92: /* $@31: %empty  */
#line 266 "./compiler.y"
                                                                                                    { fprintf_type(); fprintf(jasminFile, "and\n"); }
#line 1844 "./build/y.tab.c"
    break;

  case 93: /* Expr: ExprLeftSide $@30 BAN_ASSIGN Expr $@31  */
#line 266 "./compiler.y"
                                                                                                                                                      { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
#line 1850 "./build/y.tab.c"
    break;

  case 94: /* $@32: %empty  */
#line 267 "./compiler.y"
                   { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); }
#line 1856 "./build/y.tab.c"
    break;

  case 95: /* $@33: %empty  */
#line 267 "./compiler.y"
                                                                                                    { fprintf_type(); fprintf(jasminFile, "or\n"); }
#line 1862 "./build/y.tab.c"
    break;

  case 96: /* Expr: ExprLeftSide $@32 BOR_ASSIGN Expr $@33  */
#line 267 "./compiler.y"
                                                                                                                                                     { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
#line 1868 "./build/y.tab.c"
    break;

  case 97: /* $@34: %empty  */
#line 268 "./compiler.y"
                   { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); }
#line 1874 "./build/y.tab.c"
    break;

  case 98: /* $@35: %empty  */
#line 268 "./compiler.y"
                                                                                                    { fprintf_type(); fprintf(jasminFile, "shr\n"); }
#line 1880 "./build/y.tab.c"
    break;

  case 99: /* Expr: ExprLeftSide $@34 SHR_ASSIGN Expr $@35  */
#line 268 "./compiler.y"
                                                                                                                                                      { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
#line 1886 "./build/y.tab.c"
    break;

  case 100: /* $@36: %empty  */
#line 269 "./compiler.y"
                   { fprintf_type(); fprintf(jasminFile, "load %d\n",store_addr); }
#line 1892 "./build/y.tab.c"
    break;

  case 101: /* $@37: %empty  */
#line 269 "./compiler.y"
                                                                                                    { fprintf_type(); fprintf(jasminFile, "shl\n"); }
#line 1898 "./build/y.tab.c"
    break;

  case 102: /* Expr: ExprLeftSide $@36 SHL_ASSIGN Expr $@37  */
#line 269 "./compiler.y"
                                                                                                                                                      { fprintf_type(); fprintf(jasminFile, "store %d\n",store_addr); }
#line 1904 "./build/y.tab.c"
    break;

  case 104: /* ExprLeftSide: IDENT  */
#line 274 "./compiler.y"
            { store_addr=find_ident_address((yyvsp[0].s_var)); cur_value_type=find_ident_type((yyvsp[0].s_var)); }
#line 1910 "./build/y.tab.c"
    break;

  case 105: /* $@38: %empty  */
#line 275 "./compiler.y"
            { array_mode=1; fprintf(jasminFile, "aload %d\n",find_ident_address((yyvsp[0].s_var))); }
#line 1916 "./build/y.tab.c"
    break;

  case 106: /* ExprLeftSide: IDENT $@38 ArrayStmt  */
#line 275 "./compiler.y"
                                                                                                         {;}
#line 1922 "./build/y.tab.c"
    break;

  case 107: /* ExprLeftSide: '(' Expr ')'  */
#line 276 "./compiler.y"
                   {;}
#line 1928 "./build/y.tab.c"
    break;

  case 108: /* ExprRightSide: ExprRightSide LOR ExprDoubleAnd  */
#line 280 "./compiler.y"
                                      { fprintf_type(); fprintf(jasminFile, "or ;\n" );}
#line 1934 "./build/y.tab.c"
    break;

  case 110: /* ExprDoubleAnd: ExprDoubleAnd LAN ExprOr  */
#line 285 "./compiler.y"
                               { fprintf_type(); fprintf(jasminFile, "and ;\n" ); }
#line 1940 "./build/y.tab.c"
    break;

  case 112: /* ExprOr: ExprOr BOR ExprXor  */
#line 290 "./compiler.y"
                         { fprintf_type(); fprintf(jasminFile, "or ;\n" ); }
#line 1946 "./build/y.tab.c"
    break;

  case 114: /* ExprXor: ExprXor BXO ExprAnd  */
#line 295 "./compiler.y"
                          { fprintf_type(); fprintf(jasminFile, "xor\n" ); }
#line 1952 "./build/y.tab.c"
    break;

  case 116: /* ExprAnd: ExprAnd BAN ExprSame  */
#line 300 "./compiler.y"
                           { fprintf_type(); fprintf(jasminFile, "and\n" ); }
#line 1958 "./build/y.tab.c"
    break;

  case 118: /* ExprSame: ExprSame EQL ExprBigOrSmall  */
#line 305 "./compiler.y"
                                  { token_EQLNEQNOT(1); }
#line 1964 "./build/y.tab.c"
    break;

  case 119: /* ExprSame: ExprSame NEQ ExprBigOrSmall  */
#line 306 "./compiler.y"
                                  { token_EQLNEQNOT(2); }
#line 1970 "./build/y.tab.c"
    break;

  case 121: /* ExprBigOrSmall: ExprBigOrSmall GTR ExprBitwise  */
#line 311 "./compiler.y"
                                     { token_GTRGEQLEQLES(1); }
#line 1976 "./build/y.tab.c"
    break;

  case 122: /* ExprBigOrSmall: ExprBigOrSmall GEQ ExprBitwise  */
#line 312 "./compiler.y"
                                     { token_GTRGEQLEQLES(2); }
#line 1982 "./build/y.tab.c"
    break;

  case 123: /* ExprBigOrSmall: ExprBigOrSmall LEQ ExprBitwise  */
#line 313 "./compiler.y"
                                     { token_GTRGEQLEQLES(3); }
#line 1988 "./build/y.tab.c"
    break;

  case 124: /* ExprBigOrSmall: ExprBigOrSmall LES ExprBitwise  */
#line 314 "./compiler.y"
                                     { token_GTRGEQLEQLES(4); }
#line 1994 "./build/y.tab.c"
    break;

  case 126: /* ExprBitwise: ExprBitwise SHR ExprPlus  */
#line 319 "./compiler.y"
                               { fprintf(jasminFile, "\tishr ;\n" ); }
#line 2000 "./build/y.tab.c"
    break;

  case 127: /* ExprBitwise: ExprBitwise SHL ExprPlus  */
#line 320 "./compiler.y"
                               { fprintf(jasminFile, "\tishl ;\n" ); }
#line 2006 "./build/y.tab.c"
    break;

  case 129: /* ExprPlus: ExprPlus ADD ExprCross  */
#line 325 "./compiler.y"
                            { fprintf_type(); fprintf(jasminFile, "add ;\n" ); }
#line 2012 "./build/y.tab.c"
    break;

  case 130: /* ExprPlus: ExprPlus SUB ExprCross  */
#line 326 "./compiler.y"
                            { fprintf_type(); fprintf(jasminFile, "sub ;\n" ); }
#line 2018 "./build/y.tab.c"
    break;

  case 132: /* ExprCross: ExprCross MUL ExprFrontUnary  */
#line 331 "./compiler.y"
                                   { fprintf_type(); fprintf(jasminFile, "mul ;\n" ); }
#line 2024 "./build/y.tab.c"
    break;

  case 133: /* ExprCross: ExprCross DIV ExprFrontUnary  */
#line 332 "./compiler.y"
                                   { fprintf_type(); fprintf(jasminFile, "div ;\n" ); }
#line 2030 "./build/y.tab.c"
    break;

  case 134: /* ExprCross: ExprCross REM ExprFrontUnary  */
#line 333 "./compiler.y"
                                   { fprintf_type(); fprintf(jasminFile, "rem ;\n" );}
#line 2036 "./build/y.tab.c"
    break;

  case 136: /* ExprFrontUnary: NOT ExprFrontUnary  */
#line 338 "./compiler.y"
                         { token_EQLNEQNOT(3); }
#line 2042 "./build/y.tab.c"
    break;

  case 137: /* ExprFrontUnary: BNT ExprFrontUnary  */
#line 339 "./compiler.y"
                         { token_BNT(); }
#line 2048 "./build/y.tab.c"
    break;

  case 138: /* ExprFrontUnary: ADD ExprFrontUnary  */
#line 340 "./compiler.y"
                         {;}
#line 2054 "./build/y.tab.c"
    break;

  case 139: /* ExprFrontUnary: SUB ExprFrontUnary  */
#line 341 "./compiler.y"
                         { if(cur_value_type==7) fprintf(jasminFile, "\tineg ;\n" ); if(cur_value_type==9) fprintf(jasminFile, "\tfneg ;\n" );}
#line 2060 "./build/y.tab.c"
    break;

  case 140: /* $@39: %empty  */
#line 342 "./compiler.y"
                         { cur_addr=-1; }
#line 2066 "./build/y.tab.c"
    break;

  case 141: /* ExprFrontUnary: '(' VARIABLE_T ')' $@39 ExprPostfix  */
#line 342 "./compiler.y"
                                                      {cur_value_type=(yyvsp[-3].var_type); transformIdentType(cur_value_type);  }
#line 2072 "./build/y.tab.c"
    break;

  case 143: /* ExprPostfix: ExprValue INC_ASSIGN  */
#line 347 "./compiler.y"
                           { token_INCDEC(1); }
#line 2078 "./build/y.tab.c"
    break;

  case 144: /* ExprPostfix: ExprValue DEC_ASSIGN  */
#line 348 "./compiler.y"
                           { token_INCDEC(2); }
#line 2084 "./build/y.tab.c"
    break;

  case 146: /* ExprValue: INT_LIT  */
#line 353 "./compiler.y"
              { fprintf(jasminFile, "\tldc %d ;\n",(yyvsp[0].i_var) ); lineType=cur_value_type=7; }
#line 2090 "./build/y.tab.c"
    break;

  case 147: /* ExprValue: FLOAT_LIT  */
#line 354 "./compiler.y"
                { fprintf(jasminFile, "\tldc %f ;\n",(yyvsp[0].f_var) ); lineType=cur_value_type=9; }
#line 2096 "./build/y.tab.c"
    break;

  case 148: /* ExprValue: BOOL_LIT  */
#line 355 "./compiler.y"
               { fprintf(jasminFile, "\tldc %d ;\n",(yyvsp[0].b_var) ); lineType=cur_value_type=3; }
#line 2102 "./build/y.tab.c"
    break;

  case 149: /* ExprValue: CHAR_LIT  */
#line 356 "./compiler.y"
               { fprintf(jasminFile, "\tldc \"%c\" ;\n",(yyvsp[0].c_var) ); lineType=cur_value_type=11; }
#line 2108 "./build/y.tab.c"
    break;

  case 150: /* ExprValue: STR_LIT  */
#line 357 "./compiler.y"
              { fprintf(jasminFile, "\tldc \"%s\" ;\n",(yyvsp[0].s_var) ); lineType=cur_value_type=11; }
#line 2114 "./build/y.tab.c"
    break;

  case 151: /* ExprValue: IDENT '(' FunctionParameterStmtList ')'  */
#line 358 "./compiler.y"
                                              { fprintf(jasminFile, "\tinvokestatic Main/%s%s\n",(yyvsp[-3].s_var),find_call((yyvsp[-3].s_var)) ); cur_value_type=find_func_type(find_call((yyvsp[-3].s_var)));}
#line 2120 "./build/y.tab.c"
    break;

  case 152: /* ExprValue: IDENT  */
#line 359 "./compiler.y"
            { if(strcmp((yyvsp[0].s_var),"calculate_pi")==0){ cur_value_type=9; }else if(strcmp((yyvsp[0].s_var),"endl")==0){ cur_value_type=11; change_line=1; }else{ cur_value_type = find_ident_type((yyvsp[0].s_var)); cur_addr=find_ident_address((yyvsp[0].s_var)); fprintf_type(); fprintf(jasminFile, "load %d\n",cur_addr); } }
#line 2126 "./build/y.tab.c"
    break;

  case 153: /* $@40: %empty  */
#line 360 "./compiler.y"
            { fprintf(jasminFile, "\taload %d\n",find_ident_address((yyvsp[0].s_var))); }
#line 2132 "./build/y.tab.c"
    break;

  case 154: /* ExprValue: IDENT $@40 ArrayStmt  */
#line 360 "./compiler.y"
                                                                                             { cur_value_type = find_ident_type((yyvsp[-2].s_var)); fprintf_type(); fprintf(jasminFile, "aload\n"); }
#line 2138 "./build/y.tab.c"
    break;

  case 155: /* ExprValue: '(' Expr ')'  */
#line 361 "./compiler.y"
                   {;}
#line 2144 "./build/y.tab.c"
    break;


#line 2148 "./build/y.tab.c"

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

#line 365 "./compiler.y"

/* C code section */
void init_jasminFile(){
    jasminFile = fopen("build/Main.j", "w");

    fprintf(jasminFile, ".source Main.j\n.class public Main\n.super java/lang/Object\n");
}

void writeFunc_jasminFile(char* name , char* func_sig){
    fprintf(jasminFile, "\n.method public static %s%s\n",name,func_sig);
    fprintf(jasminFile, ".limit stack 100\n.limit locals 100\n");
}

void leaveFunc_jasminFile(int type,char* id){
    if( (type==3 || type==7 || type==9) && (strcmp(id,"main") != 0) ){
        fprintf_type(); 
    }
    fprintf(jasminFile, "return\n.end method\n");
}

SymbolT* crt_SymbolT(int insertScope){
    if(syT != NULL){
        return syT;
    }
    else{
        int size = sizeof(SymbolT);
        return (SymbolT*)malloc(size);
    }
}

void ins_SymbolT(int type,char* name,int addr,int lineno,char* func_sig,int currentScope){ 
    int size = sizeof(SymbolTColumn);
    SymbolTColumn* column = (SymbolTColumn*)malloc(size);
    column->index = find_index(currentScope);
    column->scope_level = currentScope;
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
        else{
            currentColumn = currentColumn->next;
        }
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

int find_ident_address(char* id){
    int maximum_scope = -10;
    int ans = -1;
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( strcmp(currentColumn->name,id) == 0 && currentColumn->print == 0 && currentColumn->scope_level > maximum_scope && currentColumn->addr >= 0 ){
            maximum_scope = currentColumn->scope_level;
            ans = currentColumn -> addr;
        }
        currentColumn = currentColumn->next;
    }
    return ans;
}

int find_func_type(char* sig){
    char funcType = sig[strlen(sig)-1];
    
    if(funcType == 'Z') return 3;
    else if(funcType == 'I') return 7;
    else if(funcType == 'F') return 9;
    else return 11;
}

int find_ident_type(char* id){
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if( strcmp(currentColumn->name,id) == 0 && currentColumn->type!=0 ){
            return currentColumn -> type;
        }
        else{
            currentColumn = currentColumn->next;
        }
    }
    return 9;
}

int find_ident_type_inAddr(int scope_address){
    SymbolTColumn* currentColumn = syT->head;
    while(currentColumn != NULL){
        if(currentColumn->type >= 13){
            currentColumn->type = stack_type[currentColumn->type];
        }
        if( currentColumn->addr == scope_address && currentColumn->print == 0 ){
            return currentColumn->type;
        }
        else{
            currentColumn = currentColumn->next;
        }
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
        else{
            currentColumn = currentColumn->next;
        }
    }
}

void construct_func(char* id){
    strcpy(tempFuncSig[tempFuncSig_counter],"(");
    for(int i=0 ; i < func_sig_counter ; i++){
        if(func_sig_array[i] > 12){
            strcat(tempFuncSig[tempFuncSig_counter],"[");
            func_sig_array[i] -= 12;
        }

        if( (i == func_sig_counter-1 && strcmp("main",id) == 0) || func_sig_array[i] == 2){
            strcat(tempFuncSig[tempFuncSig_counter],"V");
        }
        else if(func_sig_array[i] == 3){
            strcat(tempFuncSig[tempFuncSig_counter],"Z");
        }
        else if(func_sig_array[i] == 7){
            strcat(tempFuncSig[tempFuncSig_counter],"I");
        }
        else if(func_sig_array[i] == 9){
            strcat(tempFuncSig[tempFuncSig_counter],"F");
        }
        else if(func_sig_array[i] == 11){
            strcat(tempFuncSig[tempFuncSig_counter],"Ljava/lang/String;");
        }

        if(i == func_sig_counter-2)   strcat(tempFuncSig[tempFuncSig_counter],")");
    }
    tempFuncSig_counter++;
    func_sig_counter = 0;
}

void fprint_funcsig(){
    if(cur_value_type>=13) cur_value_type = stack_type[cur_value_type];
    if(change_line==1) fprintf(jasminFile, "\tldc \"\\n\"\n");
    fprintf(jasminFile, "\tinvokevirtual java/io/PrintStream/print(");
    if(cur_value_type <= 2)         fprintf(jasminFile, "V");
    else if(cur_value_type == 3)    fprintf(jasminFile, "Z");
    else if(cur_value_type == 4)    fprintf(jasminFile, "B");
    else if(cur_value_type == 5)    fprintf(jasminFile, "C");
    else if(cur_value_type == 6)    fprintf(jasminFile, "S");
    else if(cur_value_type == 7)    fprintf(jasminFile, "I");
    else if(cur_value_type == 8)    fprintf(jasminFile, "J");
    else if(cur_value_type == 9)    fprintf(jasminFile, "F");
    else if(cur_value_type == 10)   fprintf(jasminFile, "D");
    else if(cur_value_type == 11)   fprintf(jasminFile, "Ljava/lang/String;");
    fprintf(jasminFile, ")V\n\n");
    change_line=0;
}

char* change_num_to_type(int currentType){
    if( currentType == 1 )         return "auto";
    else if( currentType == 2 )    return "void";
    else if( currentType == 3 )    return "bool";
    else if( currentType == 4 )    return "byte";
    else if( currentType == 5 )    return "char";
    else if( currentType == 6 )    return "short";
    else if( currentType == 7 )    return "int";
    else if( currentType == 8 )    return "long";
    else if( currentType == 9 )    return "float";
    else if( currentType == 10 )   return "double";
    else if( currentType == 11 )   return "string";
    else if( currentType == 12 )   return "function";
    else                           return change_num_to_type(stack_type[currentType]);
}

void fprintf_type(){
    if(cur_value_type>=13) cur_value_type = stack_type[cur_value_type];

    if(cur_value_type==3 || cur_value_type==7)  fprintf(jasminFile, "\ti");
    else if(cur_value_type==9)                  fprintf(jasminFile, "\tf");
    else if(cur_value_type==11)                 fprintf(jasminFile, "\ta");
    else                                        fprintf(jasminFile, "wrong %d",cur_value_type);
}

void token_GTRGEQLEQLES(int action){
    if(cur_value_type>=13) cur_value_type = stack_type[cur_value_type];
    
    if(action == 1 && cur_value_type==7){
        fprintf(jasminFile, "\tif_icmpgt Loop_0_%d\n",loopNum);
    }
    else if(action == 1 && cur_value_type == 9 ){
        fprintf(jasminFile, "\tfcmpg  ;\n");
        fprintf(jasminFile, "\tifgt Loop_0_%d\n",loopNum);
        
    }
    else if(action == 2) fprintf(jasminFile, "\tif_icmpge Loop_0_%d\n",loopNum);
    else if(action == 3) fprintf(jasminFile, "\tif_icmple Loop_0_%d\n",loopNum);
    else if(action == 4) fprintf(jasminFile, "\tif_icmplt Loop_0_%d\n",loopNum);

    fprintf(jasminFile, "\ticonst_0\n");
    fprintf(jasminFile, "\tgoto Loop_1_%d\n",loopNum);
    fprintf(jasminFile, "Loop_0_%d:\n",loopNum);
    fprintf(jasminFile, "\ticonst_1\n");
    fprintf(jasminFile, "Loop_1_%d:\n",loopNum++);
}

void token_EQLNEQNOT(int action){
    if(action == 1)      fprintf(jasminFile, "\tif_icmpeq Loop_0_%d\n",loopNum);
    else if(action == 2) fprintf(jasminFile, "\tif_icmpne Loop_0_%d\n",loopNum);
    else if(action == 3) fprintf(jasminFile, "\tifne Loop_0_%d\n",loopNum);

    fprintf(jasminFile, "\ticonst_0\n");
    fprintf(jasminFile, "\tgoto Loop_1_%d\n",loopNum);
    fprintf(jasminFile, "Loop_0_%d:\n",loopNum);
    fprintf(jasminFile, "\ticonst_1\n");
    fprintf(jasminFile, "Loop_1_%d:\n",loopNum++);
}

void token_INCDEC(int action){
    fprintf(jasminFile, "\tldc 1\n");

    fprintf_type();
    if(action==1) fprintf(jasminFile, "add\n");
    else if(action==2) fprintf(jasminFile, "sub\n");

    fprintf(jasminFile, "\tistore %d\n",cur_addr);
}

void token_IFELSE(int action){
    if(action==1){
        stack_if[++stack_if_counter]=(loopNum++);
        fprintf(jasminFile, "\tifeq If_alpha_%d\n",stack_if[stack_if_counter]); 
    }
    else if(action==2){
        if(func_if==1){
            fprintf(jasminFile, "\tireturn\n");
            func_if=0;
        }
        fprintf(jasminFile, "\tgoto If_belta_%d\n",stack_if[stack_if_counter]);
        fprintf(jasminFile, "If_alpha_%d:\n",stack_if[stack_if_counter]);
    }
    else{
        fprintf(jasminFile, "If_belta_%d:\n",stack_if[stack_if_counter--]);
    }
}

void token_FOR(int action){
    if(action==1){
        fprintf(jasminFile, "f_Alpha_%d:\n",stack_for[stack_for_counter]);
    }
    else if(action==2){
        fprintf(jasminFile, "ifeq f_Delta_%d\n",stack_for[stack_for_counter]);
        fprintf(jasminFile, "\tgoto f_Gamma_%d\n",stack_for[stack_for_counter]);
        fprintf(jasminFile, "f_beta_%d:\n",stack_for[stack_for_counter]);
    }
    else{
        fprintf(jasminFile, "\tgoto f_Alpha_%d\n",stack_for[stack_for_counter]);
        fprintf(jasminFile, "f_Gamma_%d:\n",stack_for[stack_for_counter]);
    }
}

void token_BNT(){
    fprintf(jasminFile, "\ticonst_m1\n");
    fprintf(jasminFile, "\tixor\n");
}

void transformIdentType(int type){
    if(type==7){
        fprintf(jasminFile, "\tf2i\n");
        cur_value_type = type;
    }
    else if(type==9){
        fprintf(jasminFile, "\ti2f\n");
        cur_value_type = type;
    }
}
