/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_BUILD_Y_TAB_H_INCLUDED
# define YY_YY_BUILD_Y_TAB_H_INCLUDED
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
    IDENT = 300,                   /* IDENT  */
    INT_LIT = 301,                 /* INT_LIT  */
    FLOAT_LIT = 302,               /* FLOAT_LIT  */
    BOOL_LIT = 303,                /* BOOL_LIT  */
    STR_LIT = 304                  /* STR_LIT  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define COUT 258
#define SHR 259
#define SHL 260
#define BAN 261
#define BOR 262
#define BNT 263
#define BXO 264
#define ADD 265
#define SUB 266
#define MUL 267
#define DIV 268
#define REM 269
#define NOT 270
#define GTR 271
#define LES 272
#define GEQ 273
#define LEQ 274
#define EQL 275
#define NEQ 276
#define LAN 277
#define LOR 278
#define VAL_ASSIGN 279
#define ADD_ASSIGN 280
#define SUB_ASSIGN 281
#define MUL_ASSIGN 282
#define DIV_ASSIGN 283
#define REM_ASSIGN 284
#define BAN_ASSIGN 285
#define BOR_ASSIGN 286
#define BXO_ASSIGN 287
#define SHR_ASSIGN 288
#define SHL_ASSIGN 289
#define INC_ASSIGN 290
#define DEC_ASSIGN 291
#define IF 292
#define ELSE 293
#define FOR 294
#define WHILE 295
#define RETURN 296
#define BREAK 297
#define CONTINUE 298
#define VARIABLE_T 299
#define IDENT 300
#define INT_LIT 301
#define FLOAT_LIT 302
#define BOOL_LIT 303
#define STR_LIT 304

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 55 "./compiler.y"

    ObjectType var_type;

    bool b_var;
    int i_var;
    float f_var;
    char *s_var;

    Object object_var;

#line 176 "./build/y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_BUILD_Y_TAB_H_INCLUDED  */
