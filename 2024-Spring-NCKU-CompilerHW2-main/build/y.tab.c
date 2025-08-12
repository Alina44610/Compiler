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

    #include "compiler_common.h"
    #include "compiler_util.h"
    #include "main.h"

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
    int stack_type[30];
    
    int stack_cout[30];
    int cout_counter = 0;
    int type_no = 10;
    int array_length = 0;
    int cur_value_type = 1;
    int func_sig_array[30];
    int func_sig_counter = 0;
    int tempFuncSig_counter = 0;
    char tempFuncSig[10][80];
    SymbolT* syT = NULL;
    SymbolTColumn* tailColumn = NULL;

    SymbolT* crt_SymbolT(int insertScope);
    void ins_SymbolT(int type,char* name,int addr,int lineno,char* func_sig,int currentScope);
    void print_SymbolT(SymbolTColumn* currentColumn, int currentScope);
    void print_ident(char* name,int address);
    void print_cout();
    char* change_num_to_type(int type);
    char* find_call(char* id);
    int find_index(int currentScope);
    int find_ident_type(char* id);
    int find_ident_address(char* id);
    void ins_func_sig(char *id);  
    void construct_func();

#line 123 "./build/y.tab.c"

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
  YYSYMBOL_IDENT = 45,                     /* IDENT  */
  YYSYMBOL_INT_LIT = 46,                   /* INT_LIT  */
  YYSYMBOL_FLOAT_LIT = 47,                 /* FLOAT_LIT  */
  YYSYMBOL_BOOL_LIT = 48,                  /* BOOL_LIT  */
  YYSYMBOL_STR_LIT = 49,                   /* STR_LIT  */
  YYSYMBOL_50_ = 50,                       /* ';'  */
  YYSYMBOL_51_ = 51,                       /* '('  */
  YYSYMBOL_52_ = 52,                       /* ')'  */
  YYSYMBOL_53_ = 53,                       /* '{'  */
  YYSYMBOL_54_ = 54,                       /* '}'  */
  YYSYMBOL_55_ = 55,                       /* ','  */
  YYSYMBOL_56_ = 56,                       /* '['  */
  YYSYMBOL_57_ = 57,                       /* ']'  */
  YYSYMBOL_58_ = 58,                       /* ':'  */
  YYSYMBOL_YYACCEPT = 59,                  /* $accept  */
  YYSYMBOL_Program = 60,                   /* Program  */
  YYSYMBOL_61_1 = 61,                      /* $@1  */
  YYSYMBOL_GlobalStmtList = 62,            /* GlobalStmtList  */
  YYSYMBOL_GlobalStmt = 63,                /* GlobalStmt  */
  YYSYMBOL_DefineVariableStmt = 64,        /* DefineVariableStmt  */
  YYSYMBOL_FunctionDefStmt = 65,           /* FunctionDefStmt  */
  YYSYMBOL_66_2 = 66,                      /* $@2  */
  YYSYMBOL_67_3 = 67,                      /* $@3  */
  YYSYMBOL_68_4 = 68,                      /* $@4  */
  YYSYMBOL_FunctionParameterStmtList = 69, /* FunctionParameterStmtList  */
  YYSYMBOL_FunctionParameterStmt = 70,     /* FunctionParameterStmt  */
  YYSYMBOL_StmtList = 71,                  /* StmtList  */
  YYSYMBOL_Stmt = 72,                      /* Stmt  */
  YYSYMBOL_73_5 = 73,                      /* $@5  */
  YYSYMBOL_74_6 = 74,                      /* $@6  */
  YYSYMBOL_75_7 = 75,                      /* $@7  */
  YYSYMBOL_76_8 = 76,                      /* $@8  */
  YYSYMBOL_77_9 = 77,                      /* $@9  */
  YYSYMBOL_RegularDefineStmt = 78,         /* RegularDefineStmt  */
  YYSYMBOL_CoutParmListStmt = 79,          /* CoutParmListStmt  */
  YYSYMBOL_IfElseStmt = 80,                /* IfElseStmt  */
  YYSYMBOL_81_10 = 81,                     /* $@10  */
  YYSYMBOL_ForStmt = 82,                   /* ForStmt  */
  YYSYMBOL_ForInit = 83,                   /* ForInit  */
  YYSYMBOL_ArrayStmt = 84,                 /* ArrayStmt  */
  YYSYMBOL_ArrayVariable = 85,             /* ArrayVariable  */
  YYSYMBOL_ArrayExpression = 86,           /* ArrayExpression  */
  YYSYMBOL_ArrayInsideExpression = 87,     /* ArrayInsideExpression  */
  YYSYMBOL_StmtParameter = 88,             /* StmtParameter  */
  YYSYMBOL_StmtParameterList = 89,         /* StmtParameterList  */
  YYSYMBOL_Expr = 90,                      /* Expr  */
  YYSYMBOL_ExprRightSide = 91,             /* ExprRightSide  */
  YYSYMBOL_ExprDoubleAnd = 92,             /* ExprDoubleAnd  */
  YYSYMBOL_ExprOr = 93,                    /* ExprOr  */
  YYSYMBOL_ExprXor = 94,                   /* ExprXor  */
  YYSYMBOL_ExprAnd = 95,                   /* ExprAnd  */
  YYSYMBOL_ExprSame = 96,                  /* ExprSame  */
  YYSYMBOL_ExprBigOrSmall = 97,            /* ExprBigOrSmall  */
  YYSYMBOL_ExprBitwise = 98,               /* ExprBitwise  */
  YYSYMBOL_ExprPlus = 99,                  /* ExprPlus  */
  YYSYMBOL_ExprCross = 100,                /* ExprCross  */
  YYSYMBOL_ExprFrontUnary = 101,           /* ExprFrontUnary  */
  YYSYMBOL_ExprPostfix = 102,              /* ExprPostfix  */
  YYSYMBOL_ExprValue = 103,                /* ExprValue  */
  YYSYMBOL_ExprLeftSide = 104              /* ExprLeftSide  */
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
#define YYLAST   443

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  59
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  46
/* YYNRULES -- Number of rules.  */
#define YYNRULES  119
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  214

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   304


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
      51,    52,     2,     2,    55,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    58,    50,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    56,     2,    57,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    53,     2,    54,     2,     2,     2,     2,
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
      45,    46,    47,    48,    49
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   107,   107,   107,   108,   112,   113,   117,   118,   122,
     127,   127,   127,   127,   131,   132,   133,   137,   138,   139,
     144,   145,   149,   150,   151,   152,   152,   153,   153,   154,
     154,   154,   155,   155,   156,   157,   158,   159,   163,   164,
     165,   166,   170,   171,   175,   176,   176,   180,   181,   185,
     186,   190,   191,   195,   196,   200,   204,   205,   206,   210,
     211,   215,   216,   220,   221,   222,   223,   224,   225,   226,
     227,   228,   229,   230,   234,   235,   239,   240,   244,   245,
     249,   250,   254,   255,   259,   260,   261,   265,   266,   267,
     268,   269,   273,   274,   275,   279,   280,   281,   285,   286,
     287,   288,   292,   293,   294,   295,   296,   297,   301,   302,
     303,   307,   308,   309,   310,   311,   312,   316,   317,   318
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
  "BREAK", "CONTINUE", "VARIABLE_T", "IDENT", "INT_LIT", "FLOAT_LIT",
  "BOOL_LIT", "STR_LIT", "';'", "'('", "')'", "'{'", "'}'", "','", "'['",
  "']'", "':'", "$accept", "Program", "$@1", "GlobalStmtList",
  "GlobalStmt", "DefineVariableStmt", "FunctionDefStmt", "$@2", "$@3",
  "$@4", "FunctionParameterStmtList", "FunctionParameterStmt", "StmtList",
  "Stmt", "$@5", "$@6", "$@7", "$@8", "$@9", "RegularDefineStmt",
  "CoutParmListStmt", "IfElseStmt", "$@10", "ForStmt", "ForInit",
  "ArrayStmt", "ArrayVariable", "ArrayExpression", "ArrayInsideExpression",
  "StmtParameter", "StmtParameterList", "Expr", "ExprRightSide",
  "ExprDoubleAnd", "ExprOr", "ExprXor", "ExprAnd", "ExprSame",
  "ExprBigOrSmall", "ExprBitwise", "ExprPlus", "ExprCross",
  "ExprFrontUnary", "ExprPostfix", "ExprValue", "ExprLeftSide", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-166)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-5)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      25,    44,    45,  -166,    57,    45,  -166,  -166,  -166,    80,
    -166,   392,    58,   392,   392,   392,   392,   -40,  -166,  -166,
    -166,  -166,   326,    -8,    86,   103,   104,   106,    15,    14,
       8,    38,    87,  -166,  -166,    17,  -166,  -166,  -166,  -166,
    -166,  -166,   334,    12,    60,    62,    66,    96,    -7,   392,
    -166,   392,   392,   392,   392,   392,   392,   392,   392,   392,
     392,   392,   392,   392,   392,   392,   392,   392,  -166,  -166,
     334,    75,    -9,  -166,    38,  -166,  -166,    64,    12,    39,
    -166,   392,   392,   392,   392,   392,   392,   392,   392,   392,
     392,    86,   103,   104,   106,    15,    14,    14,     8,     8,
       8,     8,    38,    38,    87,    87,  -166,  -166,  -166,    -5,
      67,  -166,   334,  -166,    68,   392,  -166,  -166,  -166,  -166,
    -166,  -166,  -166,  -166,  -166,  -166,  -166,  -166,    69,  -166,
    -166,    71,  -166,   318,   125,    82,  -166,  -166,  -166,   376,
      81,    84,    90,  -166,   184,  -166,    89,  -166,   392,     1,
     392,   318,    85,    91,  -166,    93,  -166,  -166,   -17,    92,
    -166,  -166,  -166,  -166,    38,   392,  -166,    88,  -166,  -166,
      95,   384,   392,  -166,   392,   -16,    99,    38,  -166,   318,
      79,    94,   101,  -166,    96,   100,   117,  -166,   318,   202,
     392,   102,   392,   107,   392,  -166,  -166,  -166,  -166,   318,
     108,   109,    26,    38,   251,   392,   318,  -166,   392,  -166,
    -166,   269,    38,  -166
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       2,     0,     0,     1,     0,     3,     6,     7,     8,    10,
       5,     0,     0,     0,     0,     0,     0,   117,   111,   112,
     113,   114,     0,     0,    75,    77,    79,    81,    83,    86,
      91,    94,    97,   101,   107,   110,   116,    11,   103,   104,
     105,   102,    16,     0,   118,     0,     0,    73,   116,     0,
       9,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,   109,
      16,     0,     0,    15,    19,    53,    54,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,    76,    78,    80,    82,    84,    85,    88,    90,
      87,    89,    92,    93,    95,    96,    98,    99,   100,     0,
      17,   115,     0,    52,     0,     0,   106,    63,    64,    65,
      66,    67,    68,    71,    72,    69,    70,    12,     0,    14,
      51,     0,    18,     0,     0,     0,    27,    32,    29,     0,
       0,     0,     0,    22,     0,    21,     0,    41,     0,     0,
       0,    45,     0,     0,    36,     0,    34,    35,    62,    38,
      60,    13,    20,    23,    43,     0,    24,     0,    44,    28,
       0,    50,     0,    37,     0,    39,     0,    42,    25,     0,
      49,     0,     0,    30,    61,     0,    62,    59,    45,     0,
       0,     0,     0,     0,    58,    40,    26,    46,    48,     0,
       0,     0,     0,    57,     0,     0,     0,    55,     0,    33,
      47,     0,    56,    31
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -166,  -166,  -166,  -166,   140,  -166,  -166,  -166,  -166,  -166,
     111,    40,  -165,  -106,  -166,  -166,  -166,  -166,  -166,   -15,
    -166,   -34,  -166,  -166,  -166,     5,    98,  -166,  -166,  -166,
      -4,   -12,   -10,   116,   118,   119,   120,   131,    36,    37,
     -33,    43,   -11,   112,  -166,   -22
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     1,     2,     5,     6,     7,     8,    12,    70,   131,
      72,    73,   144,   145,   188,   151,   153,   193,   152,   146,
     149,   169,   170,   181,   182,    44,    77,   195,   202,   159,
     160,   147,    47,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      48,    23,    38,    39,    40,    41,   165,   174,   185,    74,
      46,    42,    61,    62,   189,    49,    43,    81,    82,    83,
      84,    85,    86,    87,    88,    -4,    89,    90,   102,   103,
      57,    58,    59,    60,   204,    55,    56,    74,   162,    43,
      78,   211,    50,   111,     3,   168,   112,   127,    63,    64,
     112,   166,    68,    69,   106,   107,   108,    75,    76,    48,
      48,    48,    48,    48,    48,    48,    48,    48,    48,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    74,
     207,   208,   168,   162,    17,    18,    19,    20,    21,     4,
     115,    96,    97,    48,    98,    99,   100,   101,   162,    65,
      66,    67,     9,    46,    11,   162,   104,   105,    51,    37,
      52,    48,    54,    53,    79,   164,    78,    48,    80,    49,
     110,   113,    48,   128,   133,   130,   132,   155,    48,    48,
     148,   156,   177,   150,   157,   158,   171,   190,   167,   163,
     178,   174,   172,   173,   186,    10,   191,   176,   179,    48,
      48,   192,   129,   194,   196,   199,   180,    48,   205,   201,
     183,   203,   206,   175,   184,    91,    48,    48,    48,    92,
      48,    93,   187,    94,     0,   212,   114,    48,   198,     0,
     200,   109,    48,    48,    48,    95,     0,   134,     0,    48,
       0,   116,    13,   210,    14,    15,     0,     0,     0,    16,
       0,     0,     0,     0,     0,   134,     0,     0,     0,     0,
      13,     0,    14,    15,     0,     0,     0,    16,     0,     0,
       0,   135,   136,   137,   138,   139,   140,   141,   142,    17,
      18,    19,    20,    21,   143,    22,     0,     0,   161,   135,
     136,   137,   138,   139,   140,   141,   142,    17,    18,    19,
      20,    21,   143,    22,   134,     0,   197,     0,     0,    13,
       0,    14,    15,     0,     0,     0,    16,     0,     0,     0,
       0,     0,   134,     0,     0,     0,     0,    13,     0,    14,
      15,     0,     0,     0,    16,     0,     0,     0,   135,   136,
     137,   138,   139,   140,   141,   142,    17,    18,    19,    20,
      21,   143,    22,     0,     0,   209,   135,   136,   137,   138,
     139,   140,   141,   142,    17,    18,    19,    20,    21,   143,
      22,   134,     0,   213,     0,     0,    13,     0,    14,    15,
       0,     0,     0,    16,    13,     0,    14,    15,     0,     0,
       0,    16,    13,     0,    14,    15,     0,     0,     0,    16,
       0,     0,     0,     0,     0,   135,   136,   137,   138,   139,
     140,   141,   142,    17,    18,    19,    20,    21,   143,    22,
      45,    17,    18,    19,    20,    21,     0,    22,    71,    17,
      18,    19,    20,    21,    13,    22,    14,    15,     0,     0,
       0,    16,    13,     0,    14,    15,     0,     0,     0,    16,
      13,     0,    14,    15,     0,     0,     0,    16,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    17,    18,    19,    20,    21,   154,    22,   142,    17,
      18,    19,    20,    21,     0,    22,     0,    17,    18,    19,
      20,    21,     0,    22
};

static const yytype_int16 yycheck[] =
{
      22,    11,    13,    14,    15,    16,     5,    24,    24,    42,
      22,    51,     4,     5,   179,    23,    56,    24,    25,    26,
      27,    28,    29,    30,    31,     0,    33,    34,    61,    62,
      16,    17,    18,    19,   199,    20,    21,    70,   144,    56,
      56,   206,    50,    52,     0,   151,    55,    52,    10,    11,
      55,    50,    35,    36,    65,    66,    67,    45,    46,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,   112,
      54,    55,   188,   189,    45,    46,    47,    48,    49,    44,
      51,    55,    56,   115,    57,    58,    59,    60,   204,    12,
      13,    14,    45,   115,    24,   211,    63,    64,    22,    51,
       7,   133,     6,     9,    52,   148,    56,   139,    52,    23,
      45,    57,   144,    56,    53,    57,    57,   139,   150,   151,
       5,    50,   165,    51,    50,    45,    51,    58,   150,    50,
      52,    24,    51,    50,    45,     5,    52,    55,    53,   171,
     172,    50,   112,    53,   188,    53,   171,   179,    50,    52,
     172,   194,    53,   158,   174,    49,   188,   189,   190,    51,
     192,    52,   176,    53,    -1,   208,    78,   199,   190,    -1,
     192,    70,   204,   205,   206,    54,    -1,     3,    -1,   211,
      -1,    79,     8,   205,    10,    11,    -1,    -1,    -1,    15,
      -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,    -1,
       8,    -1,    10,    11,    -1,    -1,    -1,    15,    -1,    -1,
      -1,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    -1,    -1,    54,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     3,    -1,    54,    -1,    -1,     8,
      -1,    10,    11,    -1,    -1,    -1,    15,    -1,    -1,    -1,
      -1,    -1,     3,    -1,    -1,    -1,    -1,     8,    -1,    10,
      11,    -1,    -1,    -1,    15,    -1,    -1,    -1,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    -1,    -1,    54,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     3,    -1,    54,    -1,    -1,     8,    -1,    10,    11,
      -1,    -1,    -1,    15,     8,    -1,    10,    11,    -1,    -1,
      -1,    15,     8,    -1,    10,    11,    -1,    -1,    -1,    15,
      -1,    -1,    -1,    -1,    -1,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      44,    45,    46,    47,    48,    49,    -1,    51,    44,    45,
      46,    47,    48,    49,     8,    51,    10,    11,    -1,    -1,
      -1,    15,     8,    -1,    10,    11,    -1,    -1,    -1,    15,
       8,    -1,    10,    11,    -1,    -1,    -1,    15,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    44,    45,
      46,    47,    48,    49,    -1,    51,    -1,    45,    46,    47,
      48,    49,    -1,    51
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    60,    61,     0,    44,    62,    63,    64,    65,    45,
      63,    24,    66,     8,    10,    11,    15,    45,    46,    47,
      48,    49,    51,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,    51,   101,   101,
     101,   101,    51,    56,    84,    44,    90,    91,   104,    23,
      50,    22,     7,     9,     6,    20,    21,    16,    17,    18,
      19,     4,     5,    10,    11,    12,    13,    14,    35,    36,
      67,    44,    69,    70,    99,    45,    46,    85,    56,    52,
      52,    24,    25,    26,    27,    28,    29,    30,    31,    33,
      34,    92,    93,    94,    95,    96,    97,    97,    98,    98,
      98,    98,    99,    99,   100,   100,   101,   101,   101,    69,
      45,    52,    55,    57,    85,    51,   102,    90,    90,    90,
      90,    90,    90,    90,    90,    90,    90,    52,    56,    70,
      57,    68,    57,    53,     3,    37,    38,    39,    40,    41,
      42,    43,    44,    50,    71,    72,    78,    90,     5,    79,
      51,    74,    77,    75,    50,    90,    50,    50,    45,    88,
      89,    54,    72,    50,    99,     5,    50,    90,    72,    80,
      81,    51,    51,    50,    24,    84,    55,    99,    52,    53,
      78,    82,    83,    90,    91,    24,    45,    89,    73,    71,
      58,    52,    50,    76,    53,    86,    80,    54,    90,    53,
      90,    52,    87,    99,    71,    50,    53,    54,    55,    54,
      90,    71,    99,    54
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    59,    61,    60,    60,    62,    62,    63,    63,    64,
      66,    67,    68,    65,    69,    69,    69,    70,    70,    70,
      71,    71,    72,    72,    72,    73,    72,    74,    72,    75,
      76,    72,    77,    72,    72,    72,    72,    72,    78,    78,
      78,    78,    79,    79,    80,    81,    80,    82,    82,    83,
      83,    84,    84,    85,    85,    86,    87,    87,    87,    88,
      88,    89,    89,    90,    90,    90,    90,    90,    90,    90,
      90,    90,    90,    90,    91,    91,    92,    92,    93,    93,
      94,    94,    95,    95,    96,    96,    96,    97,    97,    97,
      97,    97,    98,    98,    98,    99,    99,    99,   100,   100,
     100,   100,   101,   101,   101,   101,   101,   101,   102,   102,
     102,   103,   103,   103,   103,   103,   103,   104,   104,   104
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     0,     2,     1,     1,     1,     5,
       0,     0,     0,    11,     3,     1,     0,     2,     4,     1,
       2,     1,     1,     2,     3,     0,     6,     0,     3,     0,
       0,     9,     0,     8,     2,     2,     2,     3,     2,     3,
       5,     1,     3,     2,     1,     0,     4,     5,     3,     1,
       0,     4,     3,     1,     1,     3,     3,     1,     0,     3,
       1,     3,     1,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     1,     3,     1,     3,     1,     3,     1,
       3,     1,     3,     1,     3,     3,     1,     3,     3,     3,
       3,     1,     3,     3,     1,     3,     3,     1,     3,     3,
       3,     1,     2,     2,     2,     2,     4,     1,     2,     2,
       1,     1,     1,     1,     1,     4,     1,     1,     2,     3
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
#line 107 "./compiler.y"
      { syT=crt_SymbolT(++scope);}
#line 1413 "./build/y.tab.c"
    break;

  case 3: /* Program: $@1 GlobalStmtList  */
#line 107 "./compiler.y"
                                                  { print_SymbolT(syT->head,scope--); }
#line 1419 "./build/y.tab.c"
    break;

  case 10: /* $@2: %empty  */
#line 127 "./compiler.y"
                       { printf("func: %s\n",(yyvsp[0].s_var)); ins_SymbolT(0,(yyvsp[0].s_var),-1,yylineno,"unknown",scope); }
#line 1425 "./build/y.tab.c"
    break;

  case 11: /* $@3: %empty  */
#line 127 "./compiler.y"
                                                                                                                     { syT=crt_SymbolT(++scope); }
#line 1431 "./build/y.tab.c"
    break;

  case 12: /* $@4: %empty  */
#line 127 "./compiler.y"
                                                                                                                                                                                 { func_sig_array[func_sig_counter++]=(yyvsp[-6].var_type); construct_func(); ins_func_sig((yyvsp[-5].s_var)); }
#line 1437 "./build/y.tab.c"
    break;

  case 13: /* FunctionDefStmt: VARIABLE_T IDENT $@2 '(' $@3 FunctionParameterStmtList ')' $@4 '{' StmtList '}'  */
#line 127 "./compiler.y"
                                                                                                                                                                                                                                                                                                  { print_SymbolT(syT->head,scope--); }
#line 1443 "./build/y.tab.c"
    break;

  case 17: /* FunctionParameterStmt: VARIABLE_T IDENT  */
#line 137 "./compiler.y"
                       { ins_SymbolT((yyvsp[-1].var_type),(yyvsp[0].s_var),address++,yylineno,"-",scope); func_sig_array[func_sig_counter++]=(yyvsp[-1].var_type); }
#line 1449 "./build/y.tab.c"
    break;

  case 18: /* FunctionParameterStmt: VARIABLE_T IDENT '[' ']'  */
#line 138 "./compiler.y"
                               { ins_SymbolT((yyvsp[-3].var_type),(yyvsp[-2].s_var),address++,yylineno,"-",scope); func_sig_array[func_sig_counter++]=((yyvsp[-3].var_type)+10); }
#line 1455 "./build/y.tab.c"
    break;

  case 24: /* Stmt: COUT CoutParmListStmt ';'  */
#line 151 "./compiler.y"
                                { print_cout(); }
#line 1461 "./build/y.tab.c"
    break;

  case 25: /* $@5: %empty  */
#line 152 "./compiler.y"
                      { printf("IF\n"); }
#line 1467 "./build/y.tab.c"
    break;

  case 27: /* $@6: %empty  */
#line 153 "./compiler.y"
           { printf("ELSE\n"); }
#line 1473 "./build/y.tab.c"
    break;

  case 29: /* $@7: %empty  */
#line 154 "./compiler.y"
            { printf("WHILE\n"); }
#line 1479 "./build/y.tab.c"
    break;

  case 30: /* $@8: %empty  */
#line 154 "./compiler.y"
                                            { syT=crt_SymbolT(++scope); }
#line 1485 "./build/y.tab.c"
    break;

  case 31: /* Stmt: WHILE $@7 '(' Expr $@8 ')' '{' StmtList '}'  */
#line 154 "./compiler.y"
                                                                                               { print_SymbolT(syT->head,scope--);}
#line 1491 "./build/y.tab.c"
    break;

  case 32: /* $@9: %empty  */
#line 155 "./compiler.y"
          { printf("FOR\n"); syT=crt_SymbolT(++scope); }
#line 1497 "./build/y.tab.c"
    break;

  case 33: /* Stmt: FOR $@9 '(' ForStmt ')' '{' StmtList '}'  */
#line 155 "./compiler.y"
                                                                                          { print_SymbolT(syT->head,scope--);}
#line 1503 "./build/y.tab.c"
    break;

  case 34: /* Stmt: BREAK ';'  */
#line 156 "./compiler.y"
                { printf("BREAK\n"); }
#line 1509 "./build/y.tab.c"
    break;

  case 35: /* Stmt: CONTINUE ';'  */
#line 157 "./compiler.y"
                   { printf("CONTINUE\n"); }
#line 1515 "./build/y.tab.c"
    break;

  case 36: /* Stmt: RETURN ';'  */
#line 158 "./compiler.y"
                 { printf("RETURN\n"); }
#line 1521 "./build/y.tab.c"
    break;

  case 37: /* Stmt: RETURN Expr ';'  */
#line 159 "./compiler.y"
                      { printf("RETURN\n"); }
#line 1527 "./build/y.tab.c"
    break;

  case 38: /* RegularDefineStmt: VARIABLE_T StmtParameter  */
#line 163 "./compiler.y"
                               { if((yyvsp[-1].var_type)!=1){ cur_value_type=(yyvsp[-1].var_type); } stack_type[type_no++]=cur_value_type; }
#line 1533 "./build/y.tab.c"
    break;

  case 39: /* RegularDefineStmt: VARIABLE_T IDENT ArrayStmt  */
#line 164 "./compiler.y"
                                 { ins_SymbolT((yyvsp[-2].var_type),(yyvsp[-1].s_var),address++,yylineno,"-",scope); }
#line 1539 "./build/y.tab.c"
    break;

  case 40: /* RegularDefineStmt: VARIABLE_T IDENT ArrayStmt VAL_ASSIGN ArrayExpression  */
#line 165 "./compiler.y"
                                                            { ins_SymbolT((yyvsp[-4].var_type),(yyvsp[-3].s_var),address++,yylineno,"-",scope); }
#line 1545 "./build/y.tab.c"
    break;

  case 42: /* CoutParmListStmt: CoutParmListStmt SHL ExprPlus  */
#line 170 "./compiler.y"
                                    { stack_cout[cout_counter++]=cur_value_type; }
#line 1551 "./build/y.tab.c"
    break;

  case 43: /* CoutParmListStmt: SHL ExprPlus  */
#line 171 "./compiler.y"
                   { stack_cout[cout_counter++]=cur_value_type; }
#line 1557 "./build/y.tab.c"
    break;

  case 45: /* $@10: %empty  */
#line 176 "./compiler.y"
      { syT=crt_SymbolT(++scope); }
#line 1563 "./build/y.tab.c"
    break;

  case 46: /* IfElseStmt: $@10 '{' StmtList '}'  */
#line 176 "./compiler.y"
                                                     { print_SymbolT(syT->head,scope--);}
#line 1569 "./build/y.tab.c"
    break;

  case 53: /* ArrayVariable: IDENT  */
#line 195 "./compiler.y"
            { print_ident((yyvsp[0].s_var),find_ident_address((yyvsp[0].s_var))); }
#line 1575 "./build/y.tab.c"
    break;

  case 54: /* ArrayVariable: INT_LIT  */
#line 196 "./compiler.y"
              { printf("INT_LIT %d\n",(yyvsp[0].i_var)); }
#line 1581 "./build/y.tab.c"
    break;

  case 55: /* ArrayExpression: '{' ArrayInsideExpression '}'  */
#line 200 "./compiler.y"
                                    { printf("create array: %d\n",array_length); array_length=0; }
#line 1587 "./build/y.tab.c"
    break;

  case 56: /* ArrayInsideExpression: ArrayInsideExpression ',' ExprPlus  */
#line 204 "./compiler.y"
                                         { array_length++; }
#line 1593 "./build/y.tab.c"
    break;

  case 57: /* ArrayInsideExpression: ExprPlus  */
#line 205 "./compiler.y"
               { array_length++; }
#line 1599 "./build/y.tab.c"
    break;

  case 61: /* StmtParameterList: IDENT VAL_ASSIGN ExprRightSide  */
#line 215 "./compiler.y"
                                     { ins_SymbolT(type_no,(yyvsp[-2].s_var),address++,yylineno,"-",scope); }
#line 1605 "./build/y.tab.c"
    break;

  case 62: /* StmtParameterList: IDENT  */
#line 216 "./compiler.y"
            { ins_SymbolT(type_no,(yyvsp[0].s_var),address++,yylineno,"-",scope); }
#line 1611 "./build/y.tab.c"
    break;

  case 63: /* Expr: ExprLeftSide VAL_ASSIGN Expr  */
#line 220 "./compiler.y"
                                   { printf("EQL_ASSIGN\n"); }
#line 1617 "./build/y.tab.c"
    break;

  case 64: /* Expr: ExprLeftSide ADD_ASSIGN Expr  */
#line 221 "./compiler.y"
                                   { printf("ADD_ASSIGN\n"); }
#line 1623 "./build/y.tab.c"
    break;

  case 65: /* Expr: ExprLeftSide SUB_ASSIGN Expr  */
#line 222 "./compiler.y"
                                   { printf("SUB_ASSIGN\n"); }
#line 1629 "./build/y.tab.c"
    break;

  case 66: /* Expr: ExprLeftSide MUL_ASSIGN Expr  */
#line 223 "./compiler.y"
                                   { printf("MUL_ASSIGN\n"); }
#line 1635 "./build/y.tab.c"
    break;

  case 67: /* Expr: ExprLeftSide DIV_ASSIGN Expr  */
#line 224 "./compiler.y"
                                   { printf("DIV_ASSIGN\n"); }
#line 1641 "./build/y.tab.c"
    break;

  case 68: /* Expr: ExprLeftSide REM_ASSIGN Expr  */
#line 225 "./compiler.y"
                                   { printf("REM_ASSIGN\n"); }
#line 1647 "./build/y.tab.c"
    break;

  case 69: /* Expr: ExprLeftSide SHR_ASSIGN Expr  */
#line 226 "./compiler.y"
                                   { printf("SHR_ASSIGN\n"); }
#line 1653 "./build/y.tab.c"
    break;

  case 70: /* Expr: ExprLeftSide SHL_ASSIGN Expr  */
#line 227 "./compiler.y"
                                   { printf("SHL_ASSIGN\n"); }
#line 1659 "./build/y.tab.c"
    break;

  case 71: /* Expr: ExprLeftSide BAN_ASSIGN Expr  */
#line 228 "./compiler.y"
                                   { printf("BAN_ASSIGN\n"); }
#line 1665 "./build/y.tab.c"
    break;

  case 72: /* Expr: ExprLeftSide BOR_ASSIGN Expr  */
#line 229 "./compiler.y"
                                   { printf("BOR_ASSIGN\n"); }
#line 1671 "./build/y.tab.c"
    break;

  case 74: /* ExprRightSide: ExprRightSide LOR ExprDoubleAnd  */
#line 234 "./compiler.y"
                                      { printf("LOR\n"); }
#line 1677 "./build/y.tab.c"
    break;

  case 76: /* ExprDoubleAnd: ExprDoubleAnd LAN ExprOr  */
#line 239 "./compiler.y"
                               { printf("LAN\n"); }
#line 1683 "./build/y.tab.c"
    break;

  case 78: /* ExprOr: ExprOr BOR ExprXor  */
#line 244 "./compiler.y"
                         { printf("BOR\n"); }
#line 1689 "./build/y.tab.c"
    break;

  case 80: /* ExprXor: ExprXor BXO ExprAnd  */
#line 249 "./compiler.y"
                          { printf("BXO\n"); }
#line 1695 "./build/y.tab.c"
    break;

  case 82: /* ExprAnd: ExprAnd BAN ExprSame  */
#line 254 "./compiler.y"
                           { printf("BAN\n"); }
#line 1701 "./build/y.tab.c"
    break;

  case 84: /* ExprSame: ExprSame EQL ExprBigOrSmall  */
#line 259 "./compiler.y"
                                  { printf("EQL\n"); }
#line 1707 "./build/y.tab.c"
    break;

  case 85: /* ExprSame: ExprSame NEQ ExprBigOrSmall  */
#line 260 "./compiler.y"
                                  { printf("NEQ\n"); }
#line 1713 "./build/y.tab.c"
    break;

  case 87: /* ExprBigOrSmall: ExprBigOrSmall GEQ ExprBitwise  */
#line 265 "./compiler.y"
                                     { printf("GEQ\n"); }
#line 1719 "./build/y.tab.c"
    break;

  case 88: /* ExprBigOrSmall: ExprBigOrSmall GTR ExprBitwise  */
#line 266 "./compiler.y"
                                     { printf("GTR\n"); }
#line 1725 "./build/y.tab.c"
    break;

  case 89: /* ExprBigOrSmall: ExprBigOrSmall LEQ ExprBitwise  */
#line 267 "./compiler.y"
                                     { printf("LEQ\n"); }
#line 1731 "./build/y.tab.c"
    break;

  case 90: /* ExprBigOrSmall: ExprBigOrSmall LES ExprBitwise  */
#line 268 "./compiler.y"
                                     { printf("LES\n"); }
#line 1737 "./build/y.tab.c"
    break;

  case 92: /* ExprBitwise: ExprBitwise SHR ExprPlus  */
#line 273 "./compiler.y"
                               { printf("SHR\n"); }
#line 1743 "./build/y.tab.c"
    break;

  case 93: /* ExprBitwise: ExprBitwise SHL ExprPlus  */
#line 274 "./compiler.y"
                               { printf("SHL\n"); }
#line 1749 "./build/y.tab.c"
    break;

  case 95: /* ExprPlus: ExprPlus ADD ExprCross  */
#line 279 "./compiler.y"
                             { printf("ADD\n"); }
#line 1755 "./build/y.tab.c"
    break;

  case 96: /* ExprPlus: ExprPlus SUB ExprCross  */
#line 280 "./compiler.y"
                             { printf("SUB\n"); }
#line 1761 "./build/y.tab.c"
    break;

  case 98: /* ExprCross: ExprCross MUL ExprFrontUnary  */
#line 285 "./compiler.y"
                                   { printf("MUL\n"); }
#line 1767 "./build/y.tab.c"
    break;

  case 99: /* ExprCross: ExprCross DIV ExprFrontUnary  */
#line 286 "./compiler.y"
                                   { printf("DIV\n"); }
#line 1773 "./build/y.tab.c"
    break;

  case 100: /* ExprCross: ExprCross REM ExprFrontUnary  */
#line 287 "./compiler.y"
                                   { printf("REM\n"); }
#line 1779 "./build/y.tab.c"
    break;

  case 102: /* ExprFrontUnary: NOT ExprFrontUnary  */
#line 292 "./compiler.y"
                         { printf("NOT\n"); }
#line 1785 "./build/y.tab.c"
    break;

  case 103: /* ExprFrontUnary: BNT ExprFrontUnary  */
#line 293 "./compiler.y"
                         { printf("BNT\n"); }
#line 1791 "./build/y.tab.c"
    break;

  case 104: /* ExprFrontUnary: ADD ExprFrontUnary  */
#line 294 "./compiler.y"
                         { printf("ADD\n"); }
#line 1797 "./build/y.tab.c"
    break;

  case 105: /* ExprFrontUnary: SUB ExprFrontUnary  */
#line 295 "./compiler.y"
                         { printf("NEG\n"); }
#line 1803 "./build/y.tab.c"
    break;

  case 106: /* ExprFrontUnary: '(' VARIABLE_T ')' ExprPostfix  */
#line 296 "./compiler.y"
                                     { printf("Cast to %s\n", change_num_to_type((yyvsp[-2].var_type))); }
#line 1809 "./build/y.tab.c"
    break;

  case 108: /* ExprPostfix: ExprValue INC_ASSIGN  */
#line 301 "./compiler.y"
                           { printf("INC_ASSIGN\n"); }
#line 1815 "./build/y.tab.c"
    break;

  case 109: /* ExprPostfix: ExprValue DEC_ASSIGN  */
#line 302 "./compiler.y"
                           { printf("DEC_ASSIGN\n"); }
#line 1821 "./build/y.tab.c"
    break;

  case 111: /* ExprValue: INT_LIT  */
#line 307 "./compiler.y"
              { printf("INT_LIT %d\n",(yyvsp[0].i_var)); cur_value_type = 4; }
#line 1827 "./build/y.tab.c"
    break;

  case 112: /* ExprValue: FLOAT_LIT  */
#line 308 "./compiler.y"
                { printf("FLOAT_LIT %f\n",(yyvsp[0].f_var)); cur_value_type = 6; }
#line 1833 "./build/y.tab.c"
    break;

  case 113: /* ExprValue: BOOL_LIT  */
#line 309 "./compiler.y"
               { printf("BOOL_LIT "); ((yyvsp[0].b_var)==0)?printf("FALSE\n"):printf("TRUE\n"); cur_value_type = 8; }
#line 1839 "./build/y.tab.c"
    break;

  case 114: /* ExprValue: STR_LIT  */
#line 310 "./compiler.y"
              { printf("STR_LIT \"%s\"\n",(yyvsp[0].s_var)); cur_value_type = 9; }
#line 1845 "./build/y.tab.c"
    break;

  case 115: /* ExprValue: IDENT '(' FunctionParameterStmtList ')'  */
#line 311 "./compiler.y"
                                              { print_ident((yyvsp[-3].s_var),-1); printf("call: %s%s\n",(yyvsp[-3].s_var),find_call((yyvsp[-3].s_var))); }
#line 1851 "./build/y.tab.c"
    break;

  case 117: /* ExprLeftSide: IDENT  */
#line 316 "./compiler.y"
            { print_ident((yyvsp[0].s_var),find_ident_address((yyvsp[0].s_var))); cur_value_type = find_ident_type((yyvsp[0].s_var)); }
#line 1857 "./build/y.tab.c"
    break;

  case 118: /* ExprLeftSide: IDENT ArrayStmt  */
#line 317 "./compiler.y"
                      { print_ident((yyvsp[-1].s_var),find_ident_address((yyvsp[-1].s_var))); cur_value_type = find_ident_type((yyvsp[-1].s_var)); }
#line 1863 "./build/y.tab.c"
    break;

  case 119: /* ExprLeftSide: '(' Expr ')'  */
#line 318 "./compiler.y"
                   { pushFunInParm(&(yyvsp[-2].object_var)); }
#line 1869 "./build/y.tab.c"
    break;


#line 1873 "./build/y.tab.c"

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

#line 321 "./compiler.y"

/* C code section */
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

void print_SymbolT(SymbolTColumn* currentColumn, int currentScope){
    printf("\n");
    printf("> Dump symbol table (scope level: %d)\n",currentScope);
    printf("%-10s%-20s%-10s%-10s%-10s%-10s\n","Index","Name","Type","Addr","Lineno","Func_sig");
    
    if(currentColumn != NULL){
        do{
            if( currentColumn->scope_level == currentScope && currentColumn->print == 0 ){
                currentColumn->print = 1;
                printf("%-10d%-20s%-10s%-10d%-10d%-10s\n", currentColumn->index, currentColumn->name, change_num_to_type(currentColumn->type), currentColumn->addr, currentColumn->lineno, currentColumn->func_sig);
            }
            currentColumn = currentColumn->next;
        }while(currentColumn != NULL);
    }
}

void print_ident(char* name,int address){
    printf("IDENT (name=%s, address=%d)\n",name,address);
}

void print_cout(){
    printf("cout"); 
    for(int i=0; i<cout_counter ; i++){
        if( stack_cout[i] > 9 )          stack_cout[i]=stack_type[stack_cout[i]];

        if( stack_cout[i] == 3 )         printf(" char");
        else if( stack_cout[i] == 4 )    printf(" int");
        else if( stack_cout[i] == 6 )    printf(" float");
        else if( stack_cout[i] == 8 )    printf(" bool");
        else if( stack_cout[i] == 9 )    printf(" string");
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
