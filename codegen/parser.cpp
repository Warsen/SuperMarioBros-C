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

/* First part of user prologue.  */

#include <fstream>
#include <cstdio>
#include <cstdlib>

#include "ast.hpp"
#include "translator.hpp"

extern int yylex();
extern FILE* yyin;
extern int yylineno;

RootNode* root = nullptr;

void yyerror(const char* s);

// Short hand for an instruction AST node
// there are so many of them, so this saves some typing
//
#define INST(t, r, c, o) \
    r = new InstructionNode(c, o); (r)->lineNumber = (t).first_line;

#define YY_CAST(Type, Val) static_cast<Type> (Val)

#include "parser.hpp"

/* Symbol kind.  */
enum yysymbol_kind_t
{
	YYSYMBOL_YYEMPTY = -2,
	YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
	YYSYMBOL_YYerror = 1,                    /* error  */
	YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
	YYSYMBOL_DIRECTIVE = 3,                  /* DIRECTIVE  */
	YYSYMBOL_NAME = 4,                       /* NAME  */
	YYSYMBOL_LABEL = 5,                      /* LABEL  */
	YYSYMBOL_DATABYTES = 6,                  /* DATABYTES  */
	YYSYMBOL_DATAWORDS = 7,                  /* DATAWORDS  */
	YYSYMBOL_HEXCONST = 8,                   /* HEXCONST  */
	YYSYMBOL_BINCONST = 9,                   /* BINCONST  */
	YYSYMBOL_DECCONST = 10,                  /* DECCONST  */
	YYSYMBOL_LDA = 11,                       /* LDA  */
	YYSYMBOL_LDX = 12,                       /* LDX  */
	YYSYMBOL_LDY = 13,                       /* LDY  */
	YYSYMBOL_STA = 14,                       /* STA  */
	YYSYMBOL_STX = 15,                       /* STX  */
	YYSYMBOL_STY = 16,                       /* STY  */
	YYSYMBOL_TAX = 17,                       /* TAX  */
	YYSYMBOL_TAY = 18,                       /* TAY  */
	YYSYMBOL_TXA = 19,                       /* TXA  */
	YYSYMBOL_TYA = 20,                       /* TYA  */
	YYSYMBOL_TSX = 21,                       /* TSX  */
	YYSYMBOL_TXS = 22,                       /* TXS  */
	YYSYMBOL_PHA = 23,                       /* PHA  */
	YYSYMBOL_PHP = 24,                       /* PHP  */
	YYSYMBOL_PLA = 25,                       /* PLA  */
	YYSYMBOL_PLP = 26,                       /* PLP  */
	YYSYMBOL_AND = 27,                       /* AND  */
	YYSYMBOL_EOR = 28,                       /* EOR  */
	YYSYMBOL_ORA = 29,                       /* ORA  */
	YYSYMBOL_BIT = 30,                       /* BIT  */
	YYSYMBOL_ADC = 31,                       /* ADC  */
	YYSYMBOL_SBC = 32,                       /* SBC  */
	YYSYMBOL_CMP = 33,                       /* CMP  */
	YYSYMBOL_CPX = 34,                       /* CPX  */
	YYSYMBOL_CPY = 35,                       /* CPY  */
	YYSYMBOL_INC = 36,                       /* INC  */
	YYSYMBOL_INX = 37,                       /* INX  */
	YYSYMBOL_INY = 38,                       /* INY  */
	YYSYMBOL_DEC = 39,                       /* DEC  */
	YYSYMBOL_DEX = 40,                       /* DEX  */
	YYSYMBOL_DEY = 41,                       /* DEY  */
	YYSYMBOL_ASL = 42,                       /* ASL  */
	YYSYMBOL_LSR = 43,                       /* LSR  */
	YYSYMBOL_ROL = 44,                       /* ROL  */
	YYSYMBOL_ROR = 45,                       /* ROR  */
	YYSYMBOL_JMP = 46,                       /* JMP  */
	YYSYMBOL_JSR = 47,                       /* JSR  */
	YYSYMBOL_RTS = 48,                       /* RTS  */
	YYSYMBOL_BCC = 49,                       /* BCC  */
	YYSYMBOL_BCS = 50,                       /* BCS  */
	YYSYMBOL_BEQ = 51,                       /* BEQ  */
	YYSYMBOL_BMI = 52,                       /* BMI  */
	YYSYMBOL_BNE = 53,                       /* BNE  */
	YYSYMBOL_BPL = 54,                       /* BPL  */
	YYSYMBOL_BVC = 55,                       /* BVC  */
	YYSYMBOL_BVS = 56,                       /* BVS  */
	YYSYMBOL_CLC = 57,                       /* CLC  */
	YYSYMBOL_CLD = 58,                       /* CLD  */
	YYSYMBOL_CLI = 59,                       /* CLI  */
	YYSYMBOL_CLV = 60,                       /* CLV  */
	YYSYMBOL_SEC = 61,                       /* SEC  */
	YYSYMBOL_SED = 62,                       /* SED  */
	YYSYMBOL_SEI = 63,                       /* SEI  */
	YYSYMBOL_BRK = 64,                       /* BRK  */
	YYSYMBOL_NOP = 65,                       /* NOP  */
	YYSYMBOL_RTI = 66,                       /* RTI  */
	YYSYMBOL_67_ = 67,                       /* '='  */
	YYSYMBOL_68_ = 68,                       /* ','  */
	YYSYMBOL_69_ = 69,                       /* '#'  */
	YYSYMBOL_70_ = 70,                       /* '+'  */
	YYSYMBOL_71_ = 71,                       /* '-'  */
	YYSYMBOL_72_ = 72,                       /* '<'  */
	YYSYMBOL_73_ = 73,                       /* '>'  */
	YYSYMBOL_74_ = 74,                       /* '('  */
	YYSYMBOL_75_ = 75,                       /* ')'  */
	YYSYMBOL_76_x_ = 76,                     /* 'x'  */
	YYSYMBOL_77_y_ = 77,                     /* 'y'  */
	YYSYMBOL_YYACCEPT = 78,                  /* $accept  */
	YYSYMBOL_program = 79,                   /* program  */
	YYSYMBOL_plist = 80,                     /* plist  */
	YYSYMBOL_dir = 81,                       /* dir  */
	YYSYMBOL_decl = 82,                      /* decl  */
	YYSYMBOL_section = 83,                   /* section  */
	YYSYMBOL_code = 84,                      /* code  */
	YYSYMBOL_data = 85,                      /* data  */
	YYSYMBOL_dlist = 86,                     /* dlist  */
	YYSYMBOL_inst = 87,                      /* inst  */
	YYSYMBOL_const = 88,                     /* const  */
	YYSYMBOL_expr = 89,                      /* expr  */
	YYSYMBOL_iexpr = 90                      /* iexpr  */
};
typedef yysymbol_kind_t yysymbol_kind_t;

/* Narrow types that promote to a signed type and that can represent a
  signed or unsigned integer of at least N bits.  In tables they can
  save space and decrease cache pressure.  Promoting to a signed type
  helps avoid bugs in integer arithmetic.  */

typedef signed char yytype_int8;
typedef short yytype_int16;
typedef unsigned char yytype_uint8;
typedef unsigned short yytype_uint16;

#   include <cstddef> /* INFRINGES ON USER NAME SPACE */
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX

#define YYSIZE_T unsigned

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

#define YY_(Msgid) Msgid
#define YY_ATTRIBUTE_UNUSED

/* Suppress unused-variable warnings by "using" E.  */
# define YY_USE(E) ((void) (E))

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END

# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END

#define YY_ASSERT(E) ((void) (0 && (E)))


/* The parser invokes alloca or malloc; define the necessary symbols.  */
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#   define YYMALLOC malloc
#   define YYFREE free


/* A type that is properly aligned for any stack member.  */
union yyalloc
{
	yy_state_t yyss_alloc;
	YYSTYPE yyvs_alloc;
	YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)


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
        (Stack) = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*(Stack)) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

	  /* Copy COUNT objects from SRC to DST.  The source and destination do
		 not overlap.  */

#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)


/* YYFINAL -- State number of the termination state.  */
constexpr auto YYFINAL = 76;

/* YYLAST -- Last index in YYTABLE.  */
constexpr auto YYLAST = 234;

/* YYNTOKENS -- Number of terminals.  */
constexpr auto YYNTOKENS = 78;

/* YYNSTATES -- Number of states.  */
constexpr auto YYNSTATES = 138;

/* YYMAXUTOK -- Last valid token kind.  */
constexpr auto YYMAXUTOK = 321;

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
	   2,     2,     2,     2,     2,    69,     2,     2,     2,     2,
	  74,    75,     2,    70,    68,    71,     2,     2,     2,     2,
	   2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	  72,    67,    73,     2,     2,     2,     2,     2,     2,     2,
	   2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	   2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	   2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	   2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	   2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	  76,    77,     2,     2,     2,     2,     2,     2,     2,     2,
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
	  45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
	  55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
	  65,    66
};

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char* yysymbol_name(yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char* const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "DIRECTIVE", "NAME",
  "LABEL", "DATABYTES", "DATAWORDS", "HEXCONST", "BINCONST", "DECCONST",
  "LDA", "LDX", "LDY", "STA", "STX", "STY", "TAX", "TAY", "TXA", "TYA",
  "TSX", "TXS", "PHA", "PHP", "PLA", "PLP", "AND", "EOR", "ORA", "BIT",
  "ADC", "SBC", "CMP", "CPX", "CPY", "INC", "INX", "INY", "DEC", "DEX",
  "DEY", "ASL", "LSR", "ROL", "ROR", "JMP", "JSR", "RTS", "BCC", "BCS",
  "BEQ", "BMI", "BNE", "BPL", "BVC", "BVS", "CLC", "CLD", "CLI", "CLV",
  "SEC", "SED", "SEI", "BRK", "NOP", "RTI", "'='", "','", "'#'", "'+'",
  "'-'", "'<'", "'>'", "'('", "')'", "'x'", "'y'", "$accept", "program",
  "plist", "dir", "decl", "section", "code", "data", "dlist", "inst",
  "const", "expr", "iexpr", nullptr
};

#define YYPACT_NINF (-68)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
	  20,    18,   -58,   107,    10,    20,   -68,   -68,   -68,   -68,
	 -68,   -68,   -68,     3,     3,     3,     3,     3,     3,     3,
	   3,     3,   -68,   -68,   -68,   -68,   -68,   -68,   -68,   -68,
	 -68,   -68,     3,     3,     3,     3,     3,     3,     3,     3,
	   3,     3,   -68,   -68,     3,   -68,   -68,     3,     3,     3,
	   3,     3,    16,   -68,    35,    39,    47,    48,    49,    50,
	  51,    52,   -68,   -68,   -68,   -68,   -68,   -68,   -68,   -68,
	 -68,   -68,   -68,   168,   -68,   -68,   -68,   -68,   -68,   -68,
	 -68,     3,     3,     3,     3,   -68,   -65,   -11,   -65,   -11,
	 -49,   -68,   -68,   -68,   -68,   -68,   -68,   -68,   -68,   -68,
	 -68,   -68,   -68,   -68,   -68,   -68,   -68,   -68,   -68,   -68,
	 -68,   -68,   -68,   -68,   -68,   -68,   -68,   -68,   -68,   -68,
	 -68,   -68,   -68,   -68,   -65,   -65,   -65,   -67,     3,     3,
	   3,   -27,   -68,   -65,   -65,   -65,   -68,   -68
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
	   0,     0,     0,     0,     0,     2,     3,     4,     5,    81,
	  82,    83,     9,     0,     0,     0,     0,     0,     0,     0,
	   0,     0,    27,    28,    29,    30,    31,    32,    33,    34,
	  35,    36,     0,     0,     0,     0,     0,     0,     0,     0,
	   0,     0,    47,    48,     0,    50,    51,    52,    54,    56,
	  58,     0,     0,    62,     0,     0,     0,     0,     0,     0,
	   0,     0,    71,    72,    73,    74,    75,    76,    77,    78,
	  79,    80,    12,    11,    14,    13,     1,     6,     7,     8,
	  84,     0,     0,     0,     0,    85,    10,    17,    19,    18,
	  92,    21,    22,    23,    24,    25,    26,    37,    38,    39,
	  40,    41,    42,    43,    44,    45,    46,    49,    53,    55,
	  57,    59,    60,    61,    63,    64,    65,    66,    67,    68,
	  69,    70,    16,    15,    86,    89,    90,     0,     0,     0,
	   0,     0,    91,    87,    88,    20,    93,    94
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
	 -68,   -68,   -68,    53,    54,    37,   -68,   -12,    45,   -10,
	  61,   -13,    -3
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
	   0,     4,     5,     6,     7,     8,    73,    74,    87,    75,
	  85,    90,    91
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
	  86,    88,    88,   128,   129,   128,   129,    80,   132,    13,
	  76,     9,    10,    11,    92,    93,    94,    95,    96,   131,
	 113,   128,   129,     1,     2,     3,     9,    10,    11,    97,
	  98,    99,   100,   101,   102,   103,   104,   105,   106,   114,
	  72,   107,    79,   115,   108,   109,   110,   111,   112,   136,
	 137,   116,   117,   118,   119,   120,   121,   130,    77,    78,
	  89,   122,    12,   123,     0,     0,     0,     0,   124,   125,
	 126,   127,    81,     0,     0,    82,    83,    84,     0,     0,
	   0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
	   0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
	   0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
	   0,     0,     3,    14,    15,   133,   134,   135,    16,    17,
	  18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
	  28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
	  38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
	  48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
	  58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
	  68,    69,    70,    71,    14,    15,     0,     0,     0,    16,
	  17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
	  27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
	  37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
	  47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
	  57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
	  67,    68,    69,    70,    71
};

static const yytype_int16 yycheck[] =
{
	  13,    14,    15,    70,    71,    70,    71,     4,    75,    67,
	   0,     8,     9,    10,    17,    18,    19,    20,    21,    68,
	   4,    70,    71,     3,     4,     5,     8,     9,    10,    32,
	  33,    34,    35,    36,    37,    38,    39,    40,    41,     4,
	   3,    44,     5,     4,    47,    48,    49,    50,    51,    76,
	  77,     4,     4,     4,     4,     4,     4,    68,     5,     5,
	  15,    73,     1,    73,    -1,    -1,    -1,    -1,    81,    82,
	  83,    84,    69,    -1,    -1,    72,    73,    74,    -1,    -1,
	  -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
	  -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
	  -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
	  -1,    -1,     5,     6,     7,   128,   129,   130,    11,    12,
	  13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
	  23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
	  33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
	  43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
	  53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
	  63,    64,    65,    66,     6,     7,    -1,    -1,    -1,    11,
	  12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
	  22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
	  32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
	  42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
	  52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
	  62,    63,    64,    65,    66
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
	   0,     3,     4,     5,    79,    80,    81,    82,    83,     8,
	   9,    10,    88,    67,     6,     7,    11,    12,    13,    14,
	  15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
	  25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
	  35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
	  45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
	  55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
	  65,    66,    83,    84,    85,    87,     0,    81,    82,    83,
	   4,    69,    72,    73,    74,    88,    89,    86,    89,    86,
	  89,    90,    90,    90,    90,    90,    90,    90,    90,    90,
	  90,    90,    90,    90,    90,    90,    90,    90,    90,    90,
	  90,    90,    90,     4,     4,     4,     4,     4,     4,     4,
	   4,     4,    85,    87,    89,    89,    89,    89,    70,    71,
	  68,    68,    75,    89,    89,    89,    76,    77
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
	   0,    78,    79,    80,    80,    80,    80,    80,    80,    81,
	  82,    83,    83,    84,    84,    84,    84,    85,    85,    86,
	  86,    87,    87,    87,    87,    87,    87,    87,    87,    87,
	  87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
	  87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
	  87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
	  87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
	  87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
	  87,    88,    88,    88,    89,    89,    89,    89,    89,    89,
	  89,    89,    90,    90,    90
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
	   0,     2,     1,     1,     1,     1,     2,     2,     2,     2,
	   3,     2,     2,     1,     1,     2,     2,     2,     2,     1,
	   3,     2,     2,     2,     2,     2,     2,     1,     1,     1,
	   1,     1,     1,     1,     1,     1,     1,     2,     2,     2,
	   2,     2,     2,     2,     2,     2,     2,     1,     1,     2,
	   1,     1,     1,     2,     1,     2,     1,     2,     1,     2,
	   2,     2,     1,     2,     2,     2,     2,     2,     2,     2,
	   2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
	   1,     1,     1,     1,     1,     1,     2,     3,     3,     2,
	   2,     3,     1,     3,     3
};

enum { YYENOMEM = -2 };

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYNOMEM         goto yyexhaustedlab

   /* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
	  If N is 0, then set CURRENT to the empty location which ends
	  the previous symbol: RHS[0] (always defined).  */

# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)

#define YYRHSLOC(Rhs, K) ((Rhs)[K])

# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)

/* YYINITDEPTH -- initial size of the parser's stacks.  */
constexpr auto YYINITDEPTH = 200;

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

constexpr auto YYMAXDEPTH = 10000;

   /* Context of a parse error.  */
typedef struct
{
	yy_state_t* yyssp;
	yysymbol_kind_t yytoken;
	YYLTYPE* yylloc;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens(const yypcontext_t* yyctx,
	yysymbol_kind_t yyarg[], int yyargn)
{
	/* Actual size of YYARG. */
	int yycount = 0;
	int yyn = yypact[+*yyctx->yyssp];
	if (!yypact_value_is_default(yyn))
	{
		/* Start YYX at -YYN if negative to avoid negative indexes in
		   YYCHECK.  In other words, skip the first -YYN actions for
		   this state because they are default actions.  */
		int yyxbegin = yyn < 0 ? -yyn : 0;
		/* Stay within bounds of both yycheck and yytname.  */
		int yychecklim = YYLAST - yyn + 1;
		int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
		for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
			if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror)
			{
				if (!yyarg)
					++yycount;
				else if (yycount == yyargn)
					return 0;
				else
					yyarg[yycount++] = YY_CAST(yysymbol_kind_t, yyx);
			}
	}
	if (yyarg && yycount == 0 && 0 < yyargn)
		yyarg[0] = YYSYMBOL_YYEMPTY;
	return yycount;
}

/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen(const char* yystr)
{
	YYPTRDIFF_T yylen;
	for (yylen = 0; yystr[yylen]; yylen++)
		continue;
	return yylen;
}

/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char*
yystpcpy(char* yydest, const char* yysrc)
{
	char* yyd = yydest;
	const char* yys = yysrc;

	while ((*yyd++ = *yys++) != '\0')
		continue;

	return yyd - 1;
}

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr(char* yyres, const char* yystr)
{
	if (*yystr == '"')
	{
		YYPTRDIFF_T yyn = 0;
		char const* yyp = yystr;
		for (;;)
			switch (*++yyp)
			{
			case '\'':
			case ',':
				goto do_not_strip_quotes;

			case '\\':
				if (*++yyp != '\\')
					goto do_not_strip_quotes;

			default:
				if (yyres)
					yyres[yyn] = *yyp;
				yyn++;
				break;

			case '"':
				if (yyres)
					yyres[yyn] = '\0';
				return yyn;
			}
	do_not_strip_quotes:;
	}

	if (yyres)
		return yystpcpy(yyres, yystr) - yyres;

	return yystrlen(yystr);
}
#endif

static int
yy_syntax_error_arguments(const yypcontext_t* yyctx,
	yysymbol_kind_t yyarg[], int yyargn)
{
	/* Actual size of YYARG. */
	int yycount = 0;
	/* There are many possibilities here to consider:
	   - If this state is a consistent state with a default action, then
		 the only way this function was invoked is if the default action
		 is an error action.  In that case, don't check for expected
		 tokens because there are none.
	   - The only way there can be no lookahead present (in yychar) is if
		 this state is a consistent state with a default action.  Thus,
		 detecting the absence of a lookahead is sufficient to determine
		 that there is no unexpected or expected token to report.  In that
		 case, just report a simple "syntax error".
	   - Don't assume there isn't a lookahead just because this state is a
		 consistent state with a default action.  There might have been a
		 previous inconsistent state, consistent state with a non-default
		 action, or user semantic action that manipulated yychar.
	   - Of course, the expected token list depends on states to have
		 correct lookahead information, and it depends on the parser not
		 to perform extra reductions after fetching a lookahead from the
		 scanner and before detecting a syntax error.  Thus, state merging
		 (from LALR or IELR) and default reductions corrupt the expected
		 token list.  However, the list is correct for canonical LR with
		 one exception: it will still contain any token that will not be
		 accepted due to an error action in a later state.
	*/
	if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
	{
		if (yyarg)
			yyarg[yycount] = yyctx->yytoken;
		++yycount;
		int yyn = yypcontext_expected_tokens(yyctx, yyarg ? yyarg + 1 : yyarg, yyargn - 1);
		if (yyn == YYENOMEM)
			return YYENOMEM;
		yycount += yyn;
	}
	return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error(YYPTRDIFF_T* yymsg_alloc, char** yymsg,
	const yypcontext_t* yyctx)
{
	enum { YYARGS_MAX = 5 };
	/* Internationalized format string. */
	const char* yyformat = nullptr;
	/* Arguments of yyformat: reported tokens (one for the "unexpected",
	   one per "expected"). */
	yysymbol_kind_t yyarg[YYARGS_MAX];
	/* Cumulated lengths of YYARG.  */

	/* Actual size of YYARG. */
	int yycount = yy_syntax_error_arguments(yyctx, yyarg, YYARGS_MAX);
	if (yycount == YYENOMEM)
		return YYENOMEM;

	switch (yycount)
	{
	default: /* Avoid compiler warnings. */
		case 0: yyformat = "syntax error"; break;
		case 1: yyformat = "syntax error, unexpected %s"; break;
		case 2: yyformat = "syntax error, unexpected %s, expecting %s"; break;
		case 3: yyformat = "syntax error, unexpected %s, expecting %s or %s"; break;
		case 4: yyformat = "syntax error, unexpected %s, expecting %s or %s or %s"; break;
		case 5: yyformat = "syntax error, unexpected %s, expecting %s or %s or %s or %s"; break;
	}

	/* Compute error message size.  Don't count the "%s"s, but reserve
	   room for the terminator.  */
	YYPTRDIFF_T yysize = yystrlen(yyformat) - 2 * yycount + 1;
	{
		for (int yyi = 0; yyi < yycount; ++yyi)
		{
			YYPTRDIFF_T yysize1
				= yysize + yytnamerr(nullptr, yytname[yyarg[yyi]]);
			if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
				yysize = yysize1;
			else
				return YYENOMEM;
		}
	}

	if (*yymsg_alloc < yysize)
	{
		*yymsg_alloc = 2 * yysize;
		if (!(yysize <= *yymsg_alloc
			&& *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
			*yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
		return -1;
	}

	/* Avoid sprintf, as that infringes on the user's name space.
	   Don't have undefined behavior even if the translation
	   produced a string with the wrong number of "%s"s.  */
	{
		char* yyp = *yymsg;
		int yyi = 0;
		while ((*yyp = *yyformat) != '\0')
			if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
			{
				yyp += yytnamerr(yyp, yytname[yyarg[yyi++]]);
				yyformat += 2;
			}
			else
			{
				++yyp;
				++yyformat;
			}
	}
	return 0;
}

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct(const char* yymsg,
	yysymbol_kind_t yykind, YYSTYPE* yyvaluep, YYLTYPE* yylocationp)
{
	YY_USE(yyvaluep);
	YY_USE(yylocationp);
	if (!yymsg)
		yymsg = "Deleting";
	YY_SYMBOL_PRINT(yymsg, yykind, yyvaluep, yylocationp);

	YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
		YY_USE(yykind);
	YY_IGNORE_MAYBE_UNINITIALIZED_END
}

/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Location data for the lookahead symbol.  */
YYLTYPE yylloc = { 1, 1, 1, 1 };
/* Number of syntax errors so far.  */
int yynerrs;

/*----------.
| yyparse.  |
`----------*/

int yyparse()
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
	yy_state_t* yyss = yyssa;
	yy_state_t* yyssp = yyss;

	/* The semantic value stack: array, bottom, top.  */
	YYSTYPE yyvsa[YYINITDEPTH];
	YYSTYPE* yyvs = yyvsa;
	YYSTYPE* yyvsp = yyvs;

	/* The location stack: array, bottom, top.  */
	YYLTYPE yylsa[YYINITDEPTH];
	YYLTYPE* yyls = yylsa;
	YYLTYPE* yylsp = yyls;

	int yyn;
	/* The return value of yyparse.  */
	int yyresult;
	/* Lookahead symbol kind.  */
	yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
	/* The variables used to return semantic value and location from the
	   action routines.  */
	YYSTYPE yyval;
	YYLTYPE yyloc;

	/* The locations where the error started and ended.  */
	YYLTYPE yyerror_range[3];

	/* Buffer for error messages, and its allocated size.  */
	char yymsgbuf[128];
	char* yymsg = yymsgbuf;
	YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

	/* The number of symbols on the RHS of the reduced rule.
	   Keep to zero when no symbol should be popped.  */
	int yylen = 0;

	YYDPRINTF((stderr, "Starting parse\n"));

	yychar = YYEMPTY; /* Cause a token to be read.  */

	yylsp[0] = yylloc;
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
	YYDPRINTF((stderr, "Entering state %d\n", yystate));
	YY_ASSERT(0 <= yystate && yystate < YYNSTATES);
	YY_IGNORE_USELESS_CAST_BEGIN
		* yyssp = YY_CAST(yy_state_t, yystate);
	YY_IGNORE_USELESS_CAST_END
		YY_STACK_PRINT(yyss, yyssp);

	if (yyss + yystacksize - 1 <= yyssp)
	{
		/* Get the current used size of the three stacks, in elements.  */
		YYPTRDIFF_T yysize = yyssp - yyss + 1;

		/* defined YYSTACK_RELOCATE */
		/* Extend the stack our own way.  */
		if (YYMAXDEPTH <= yystacksize)
			YYNOMEM;
		yystacksize *= 2;
		if (YYMAXDEPTH < yystacksize)
			yystacksize = YYMAXDEPTH;

		{
			yy_state_t* yyss1 = yyss;
			auto* yyptr =
				YY_CAST(union yyalloc*,
					YYSTACK_ALLOC(YY_CAST(YYSIZE_T, YYSTACK_BYTES(yystacksize))));
			if (!yyptr)
				YYNOMEM;
			YYSTACK_RELOCATE(yyss_alloc, yyss);
			YYSTACK_RELOCATE(yyvs_alloc, yyvs);
			YYSTACK_RELOCATE(yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
			if (yyss1 != yyssa)
				YYSTACK_FREE(yyss1);
		}

		yyssp = yyss + yysize - 1;
		yyvsp = yyvs + yysize - 1;
		yylsp = yyls + yysize - 1;

		YY_IGNORE_USELESS_CAST_BEGIN
			YYDPRINTF((stderr, "Stack size increased to %ld\n",
				YY_CAST(long, yystacksize)));
		YY_IGNORE_USELESS_CAST_END

			if (yyss + yystacksize - 1 <= yyssp)
				YYABORT;
	}

	if (yystate == YYFINAL)
		goto yyacceptlab;

	/*-----------.
	| yybackup.  |
	`-----------*/
	/* Do appropriate processing given the current state.  Read a
	   lookahead token if we need one and don't already have one.  */

	   /* First try to decide what to do without reference to lookahead token.  */
	yyn = yypact[yystate];
	if (yypact_value_is_default(yyn))
		goto yydefault;

	/* Not known => get a lookahead token if don't already have one.  */

	/* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
	if (yychar == YYEMPTY)
	{
		YYDPRINTF((stderr, "Reading a token\n"));
		yychar = yylex();
	}

	if (yychar <= YYEOF)
	{
		yychar = YYEOF;
		yytoken = YYSYMBOL_YYEOF;
		YYDPRINTF((stderr, "Now at end of input.\n"));
	}
	else if (yychar == YYerror)
	{
		/* The scanner already issued an error message, process directly
		   to error recovery.  But do not keep the error token as
		   lookahead, it is too special and may lead us to an endless
		   loop in error recovery. */
		yychar = YYUNDEF;
		yytoken = YYSYMBOL_YYerror;
		yyerror_range[1] = yylloc;
		goto yyerrlab1;
	}
	else
	{
		yytoken = YYTRANSLATE(yychar);
		YY_SYMBOL_PRINT("Next token is", yytoken, &yylval, &yylloc);
	}

	/* If the proper action on seeing token YYTOKEN is to reduce or to
	   detect an error, take that action.  */
	yyn += yytoken;
	if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
		goto yydefault;
	yyn = yytable[yyn];
	if (yyn <= 0)
	{
		yyn = -yyn;
		goto yyreduce;
	}

	/* Count tokens shifted since error; after three, turn off error
	   status.  */
	if (yyerrstatus)
		yyerrstatus--;

	/* Shift the lookahead token.  */
	YY_SYMBOL_PRINT("Shifting", yytoken, &yylval, &yylloc);
	yystate = yyn;
	YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
		* ++yyvsp = yylval;
	YY_IGNORE_MAYBE_UNINITIALIZED_END
		* ++yylsp = yylloc;

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
	yyval = yyvsp[1 - yylen];

	/* Default location. */
	YYLLOC_DEFAULT(yyloc, (yylsp - yylen), yylen);
	yyerror_range[1] = yyloc;
	YY_REDUCE_PRINT(yyn);
	switch (yyn)
	{
	case 4: /* plist: decl  */
	{
		root->children.push_back(yyvsp[0].node);
		yyvsp[0].node->parent = root;
	}
	break;

	case 5: /* plist: section  */
	{
		root->children.push_back(yyvsp[0].node);
		yyvsp[0].node->parent = root;
	}
	break;

	case 7: /* plist: plist decl  */
	{
		root->children.push_back(yyvsp[0].node);
		yyvsp[0].node->parent = root;
	}
	break;

	case 8: /* plist: plist section  */
	{
		root->children.push_back(yyvsp[0].node);
		yyvsp[0].node->parent = root;
	}
	break;

	case 10: /* decl: NAME '=' expr  */
	{
		yyval.node = new DeclNode(yyvsp[-2].str, yyvsp[0].node);
		yyval.node->lineNumber = yylsp[-2].first_line;
	}
	break;

	case 11: /* section: LABEL code  */
	{
		yyval.node = new LabelNode(yyvsp[-1].str, yyvsp[0].list);
		yyval.node->lineNumber = yylsp[-1].first_line;
		yyvsp[0].list->parent = yyval.node;
	}
	break;

	case 12: /* section: LABEL section  */
	{
		yyval.node = new LabelNode(yyvsp[-1].str, yyvsp[0].node);
		yyvsp[0].node->parent = yyval.node;
	}
	break;

	case 13: /* code: inst  */
	{
		yyval.list = new ListNode();
		yyval.list->value.node = yyvsp[0].instruction;
		yyvsp[0].instruction->parent = yyval.list;
	}
	break;

	case 14: /* code: data  */
	{
		yyval.list = new ListNode();
		yyval.list->value.node = yyvsp[0].node;
		yyvsp[0].node->parent = yyval.list;
	}
	break;

	case 15: /* code: code inst  */
	{
		yyval.list = new ListNode();
		yyval.list->value.node = yyvsp[0].instruction;
		yyvsp[0].instruction->parent = yyval.list;
		yyval.list->next = yyvsp[-1].list;
	}
	break;

	case 16: /* code: code data  */
	{
		yyval.list = new ListNode();
		yyval.list->value.node = yyvsp[0].node;
		yyvsp[0].node->parent = yyval.list;
		yyval.list->next = yyvsp[-1].list;
	}
	break;

	case 17: /* data: DATABYTES dlist  */
	{
		yyval.node = new AstNode(AST_DATA8);
		yyval.node->lineNumber = yylsp[-1].first_line;
		yyval.node->value.node = yyvsp[0].list;
		yyvsp[0].list->parent = yyval.node;
	}
	break;

	case 18: /* data: DATAWORDS dlist  */
	{
		yyval.node = new AstNode(AST_DATA16);
		yyval.node->lineNumber = yylsp[-1].first_line;
		yyval.node->value.node = yyvsp[0].list;
		yyvsp[0].list->parent = yyval.node;
	}
	break;

	case 19: /* dlist: expr  */
	{
		yyval.list = new ListNode();
		yyval.list->value.node = yyvsp[0].node;
		yyvsp[0].node->parent = yyval.list;
	}
	break;

	case 20: /* dlist: dlist ',' expr  */
	{
		yyval.list = new ListNode();
		yyval.list->value.node = yyvsp[0].node;
		yyvsp[0].node->parent = yyval.list;
		yyval.list->next = yyvsp[-2].list;
	}
	break;

	case 21: /* inst: LDA iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), LDA, yyvsp[0].node); }
	break;

	case 22: /* inst: LDX iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), LDX, yyvsp[0].node); }
	break;

	case 23: /* inst: LDY iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), LDY, yyvsp[0].node); }
	break;

	case 24: /* inst: STA iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), STA, yyvsp[0].node); }
	break;

	case 25: /* inst: STX iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), STX, yyvsp[0].node); }
	break;

	case 26: /* inst: STY iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), STY, yyvsp[0].node); }
	break;

	case 27: /* inst: TAX  */
	{ INST(yylsp[0], (yyval.instruction), TAX, NULL); }
	break;

	case 28: /* inst: TAY  */
	{ INST(yylsp[0], (yyval.instruction), TAY, NULL); }
	break;

	case 29: /* inst: TXA  */
	{ INST(yylsp[0], (yyval.instruction), TXA, NULL); }
	break;

	case 30: /* inst: TYA  */
	{ INST(yylsp[0], (yyval.instruction), TYA, NULL); }
	break;

	case 31: /* inst: TSX  */
	{ INST(yylsp[0], (yyval.instruction), TSX, NULL); }
	break;

	case 32: /* inst: TXS  */
	{ INST(yylsp[0], (yyval.instruction), TXS, NULL); }
	break;

	case 33: /* inst: PHA  */
	{ INST(yylsp[0], (yyval.instruction), PHA, NULL); }
	break;

	case 34: /* inst: PHP  */
	{ INST(yylsp[0], (yyval.instruction), PHP, NULL); }
	break;

	case 35: /* inst: PLA  */
	{ INST(yylsp[0], (yyval.instruction), PLA, NULL); }
	break;

	case 36: /* inst: PLP  */
	{ INST(yylsp[0], (yyval.instruction), PLP, NULL); }
	break;

	case 37: /* inst: AND iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), AND, yyvsp[0].node); }
	break;

	case 38: /* inst: EOR iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), EOR, yyvsp[0].node); }
	break;

	case 39: /* inst: ORA iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), ORA, yyvsp[0].node); }
	break;

	case 40: /* inst: BIT iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), BIT, yyvsp[0].node); }
	break;

	case 41: /* inst: ADC iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), ADC, yyvsp[0].node); }
	break;

	case 42: /* inst: SBC iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), SBC, yyvsp[0].node); }
	break;

	case 43: /* inst: CMP iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), CMP, yyvsp[0].node); }
	break;

	case 44: /* inst: CPX iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), CPX, yyvsp[0].node); }
	break;

	case 45: /* inst: CPY iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), CPY, yyvsp[0].node); }
	break;

	case 46: /* inst: INC iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), INC, yyvsp[0].node); }
	break;

	case 47: /* inst: INX  */
	{ INST(yylsp[0], (yyval.instruction), INX, NULL); }
	break;

	case 48: /* inst: INY  */
	{ INST(yylsp[0], (yyval.instruction), INY, NULL); }
	break;

	case 49: /* inst: DEC iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), DEC, yyvsp[0].node); }
	break;

	case 50: /* inst: DEX  */
	{ INST(yylsp[0], (yyval.instruction), DEX, NULL); }
	break;

	case 51: /* inst: DEY  */
	{ INST(yylsp[0], (yyval.instruction), DEY, NULL); }
	break;

	case 52: /* inst: ASL  */
	{ INST(yylsp[0], (yyval.instruction), ASL, NULL); }
	break;

	case 53: /* inst: ASL iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), ASL, yyvsp[0].node); }
	break;

	case 54: /* inst: LSR  */
	{ INST(yylsp[0], (yyval.instruction), LSR, NULL); }
	break;

	case 55: /* inst: LSR iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), LSR, yyvsp[0].node); }
	break;

	case 56: /* inst: ROL  */
	{ INST(yylsp[0], (yyval.instruction), ROL, NULL); }
	break;

	case 57: /* inst: ROL iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), ROL, yyvsp[0].node); }
	break;

	case 58: /* inst: ROR  */
	{ INST(yylsp[0], (yyval.instruction), ROR, NULL); }
	break;

	case 59: /* inst: ROR iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), ROR, yyvsp[0].node); }
	break;

	case 60: /* inst: JMP iexpr  */
	{ INST(yylsp[-1], (yyval.instruction), JMP, yyvsp[0].node); }
	break;

	case 61: /* inst: JSR NAME  */
	{ INST(yylsp[-1], (yyval.instruction), JSR, yyvsp[0].str); }
	break;

	case 62: /* inst: RTS  */
	{ INST(yylsp[0], (yyval.instruction), RTS, NULL); }
	break;

	case 63: /* inst: BCC NAME  */
	{ INST(yylsp[-1], (yyval.instruction), BCC, yyvsp[0].str); }
	break;

	case 64: /* inst: BCS NAME  */
	{ INST(yylsp[-1], (yyval.instruction), BCS, yyvsp[0].str); }
	break;

	case 65: /* inst: BEQ NAME  */
	{ INST(yylsp[-1], (yyval.instruction), BEQ, yyvsp[0].str); }
	break;

	case 66: /* inst: BMI NAME  */
	{ INST(yylsp[-1], (yyval.instruction), BMI, yyvsp[0].str); }
	break;

	case 67: /* inst: BNE NAME  */
	{ INST(yylsp[-1], (yyval.instruction), BNE, yyvsp[0].str); }
	break;

	case 68: /* inst: BPL NAME  */
	{ INST(yylsp[-1], (yyval.instruction), BPL, yyvsp[0].str); }
	break;

	case 69: /* inst: BVC NAME  */
	{ INST(yylsp[-1], (yyval.instruction), BVC, yyvsp[0].str); }
	break;

	case 70: /* inst: BVS NAME  */
	{ INST(yylsp[-1], (yyval.instruction), BVS, yyvsp[0].str); }
	break;

	case 71: /* inst: CLC  */
	{ INST(yylsp[0], (yyval.instruction), CLC, NULL); }
	break;

	case 72: /* inst: CLD  */
	{ INST(yylsp[0], (yyval.instruction), CLD, NULL); }
	break;

	case 73: /* inst: CLI  */
	{ INST(yylsp[0], (yyval.instruction), CLI, NULL); }
	break;

	case 74: /* inst: CLV  */
	{ INST(yylsp[0], (yyval.instruction), CLV, NULL); }
	break;

	case 75: /* inst: SEC  */
	{ INST(yylsp[0], (yyval.instruction), SEC, NULL); }
	break;

	case 76: /* inst: SED  */
	{ INST(yylsp[0], (yyval.instruction), SED, NULL); }
	break;

	case 77: /* inst: SEI  */
	{ INST(yylsp[0], (yyval.instruction), SEI, NULL); }
	break;

	case 78: /* inst: BRK  */
	{ INST(yylsp[0], (yyval.instruction), BRK, NULL); }
	break;

	case 79: /* inst: NOP  */
	{ INST(yylsp[0], (yyval.instruction), NOP, NULL); }
	break;

	case 80: /* inst: RTI  */
	{ INST(yylsp[0], (yyval.instruction), RTI, NULL); }
	break;

	case 81: /* const: HEXCONST  */
	{
		yyval.str = yyvsp[0].str;
	}
	break;

	case 82: /* const: BINCONST  */
	{
		yyval.str = yyvsp[0].str;
	}
	break;

	case 83: /* const: DECCONST  */
	{
		yyval.str = yyvsp[0].str;
	}
	break;

	case 84: /* expr: NAME  */
	{
		yyval.node = new AstNode(AST_NAME, yyvsp[0].str);
	}
	break;

	case 85: /* expr: const  */
	{
		yyval.node = new AstNode(AST_CONST, yyvsp[0].str);
	}
	break;

	case 86: /* expr: '#' expr  */
	{
		yyval.node = new UnaryNode(AST_IMMEDIATE, yyvsp[0].node);
	}
	break;

	case 87: /* expr: expr '+' expr  */
	{
		yyval.node = new BinaryNode(AST_ADD, yyvsp[-2].node, yyvsp[0].node);
	}
	break;

	case 88: /* expr: expr '-' expr  */
	{
		yyval.node = new BinaryNode(AST_SUBTRACT, yyvsp[-2].node, yyvsp[0].node);
	}
	break;

	case 89: /* expr: '<' expr  */
	{
		yyval.node = new UnaryNode(AST_LOBYTE, yyvsp[0].node);
	}
	break;

	case 90: /* expr: '>' expr  */
	{
		yyval.node = new UnaryNode(AST_HIBYTE, yyvsp[0].node);
	}
	break;

	case 91: /* expr: '(' expr ')'  */
	{
		yyval.node = new UnaryNode(AST_INDIRECT, yyvsp[-1].node);
	}
	break;

	case 92: /* iexpr: expr  */
	{
		yyval.node = yyvsp[0].node;
	}
	break;

	case 93: /* iexpr: expr ',' 'x'  */
	{
		yyval.node = new UnaryNode(AST_INDEXED_X, yyvsp[-2].node);
	}
	break;

	case 94: /* iexpr: expr ',' 'y'  */
	{
		yyval.node = new UnaryNode(AST_INDEXED_Y, yyvsp[-2].node);
	}
	break;

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
	YY_SYMBOL_PRINT("-> $$ =", YY_CAST(yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

	YYPOPSTACK(yylen);
	yylen = 0;

	*++yyvsp = yyval;
	*++yylsp = yyloc;

	/* Now 'shift' the result of the reduction.  Determine what state
	   that goes to, based on the state we popped back to and the rule
	   number reduced by.  */
	{
		const int yylhs = yyr1[yyn] - YYNTOKENS;
		const int yyi = yypgoto[yylhs] + *yyssp;
		yystate = 0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
			          ? yytable[yyi]
			          : yydefgoto[yylhs];
	}

	goto yynewstate;

	/*--------------------------------------.
	| yyerrlab -- here on detecting error.  |
	`--------------------------------------*/
yyerrlab:
	/* Make sure we have latest lookahead translation.  See comments at
	   user semantic actions for why this is necessary.  */
	yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE(yychar);
	/* If not already recovering from an error, report this error.  */
	if (!yyerrstatus)
	{
		++yynerrs;
		{
			yypcontext_t yyctx
				= { yyssp, yytoken, &yylloc };
			char const* yymsgp = YY_("syntax error");
			int yysyntax_error_status;
			yysyntax_error_status = yysyntax_error(&yymsg_alloc, &yymsg, &yyctx);
			if (yysyntax_error_status == 0)
				yymsgp = yymsg;
			else if (yysyntax_error_status == -1)
			{
				if (yymsg != yymsgbuf)
					YYSTACK_FREE(yymsg);
				yymsg = YY_CAST(char*,
					YYSTACK_ALLOC(YY_CAST(YYSIZE_T, yymsg_alloc)));
				if (yymsg)
				{
					yysyntax_error_status
						= yysyntax_error(&yymsg_alloc, &yymsg, &yyctx);
					yymsgp = yymsg;
				}
				else
				{
					yymsg = yymsgbuf;
					yymsg_alloc = sizeof yymsgbuf;
					yysyntax_error_status = YYENOMEM;
				}
			}
			yyerror(yymsgp);
			if (yysyntax_error_status == YYENOMEM)
				YYNOMEM;
		}
	}

	yyerror_range[1] = yylloc;
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
			yydestruct("Error: discarding",
				yytoken, &yylval, &yylloc);
			yychar = YYEMPTY;
		}
	}

	/* Else will try to reuse lookahead token after shifting the error
	   token.  */
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
		if (!yypact_value_is_default(yyn))
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

		yyerror_range[1] = *yylsp;
		yydestruct("Error: popping",
			YY_ACCESSING_SYMBOL(yystate), yyvsp, yylsp);
		YYPOPSTACK(1);
		yystate = *yyssp;
		YY_STACK_PRINT(yyss, yyssp);
	}

	YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
		* ++yyvsp = yylval;
	YY_IGNORE_MAYBE_UNINITIALIZED_END

		yyerror_range[2] = yylloc;
	++yylsp;
	YYLLOC_DEFAULT(*yylsp, yyerror_range, 2);

	/* Shift the error token.  */
	YY_SYMBOL_PRINT("Shifting", YY_ACCESSING_SYMBOL(yyn), yyvsp, yylsp);

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
	yyerror(YY_("memory exhausted"));
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
		yytoken = YYTRANSLATE(yychar);
		yydestruct("Cleanup: discarding lookahead",
			yytoken, &yylval, &yylloc);
	}
	/* Do not reclaim the symbols of the rule whose action triggered
	   this YYABORT or YYACCEPT.  */
	YYPOPSTACK(yylen);
	YY_STACK_PRINT(yyss, yyssp);
	while (yyssp != yyss)
	{
		yydestruct("Cleanup: popping",
			YY_ACCESSING_SYMBOL(+*yyssp), yyvsp, yylsp);
		YYPOPSTACK(1);
	}
#ifndef yyoverflow
	if (yyss != yyssa)
		YYSTACK_FREE(yyss);
#endif
	if (yymsg != yymsgbuf)
		YYSTACK_FREE(yymsg);
	return yyresult;
}

int main(int argc, char** argv)
{
	if (argc < 3)
	{
		printf("usage: codegen <INPUT ASM FILE> <OUTPUT ROOT DIRECTORY>\n");
		exit(1);
	}

	root = new RootNode();

	fopen_s(&yyin, argv[1], "r");
	if (!yyin)
	{
		printf("error: could not open input file %s\n", argv[1]);
		exit(1);
	}
	yyparse();
	fclose(yyin);

	cleanupAst(root);

	/*
	for (std::list<AstNode*>::iterator it = root->children.begin(); it != root->children.end(); ++it)
	{
		AstNode* node = (*it);
		printNode(node);
	}
	*/

	Translator translator(argv[1], root);

	std::string outputDir(argv[2]);

	std::string sourceFilePath = outputDir + "/source/SMB/SMB.cpp";
	std::ofstream sourceFile(sourceFilePath.c_str());
	sourceFile << translator.getSourceOutput();

	std::string dataFilePath = outputDir + "/source/SMB/SMBData.cpp";
	std::ofstream dataFile(dataFilePath.c_str());
	dataFile << translator.getDataOutput();

	std::string dataHeaderFilePath = outputDir + "/source/SMB/SMBDataPointers.hpp";
	std::ofstream dataHeaderFile(dataHeaderFilePath.c_str());
	dataHeaderFile << translator.getDataHeaderOutput();

	std::string constantHeaderFilePath = outputDir + "/source/SMB/SMBConstants.hpp";
	std::ofstream constantHeaderFile(constantHeaderFilePath.c_str());
	constantHeaderFile << translator.getConstantHeaderOutput();

	return 0;
}

[[ noreturn ]] void yyerror(const char* s)
{
	printf("Parse error: %s\n", s);
	exit(-1);
}