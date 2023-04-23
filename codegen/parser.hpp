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

#ifndef YY_YY_ASM6502_TAB_H_INCLUDED
# define YY_YY_ASM6502_TAB_H_INCLUDED
		 /* Debug traces.  */

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
enum yytokentype
{
	YYEMPTY = -2,
	YYEOF = 0,                     /* "end of file"  */
	YYerror = 256,                 /* error  */
	YYUNDEF = 257,                 /* "invalid token"  */
	DIRECTIVE = 258,               /* DIRECTIVE  */
	NAME = 259,                    /* NAME  */
	LABEL = 260,                   /* LABEL  */
	DATABYTES = 261,               /* DATABYTES  */
	DATAWORDS = 262,               /* DATAWORDS  */
	HEXCONST = 263,                /* HEXCONST  */
	BINCONST = 264,                /* BINCONST  */
	DECCONST = 265,                /* DECCONST  */
	LDA = 266,                     /* LDA  */
	LDX = 267,                     /* LDX  */
	LDY = 268,                     /* LDY  */
	STA = 269,                     /* STA  */
	STX = 270,                     /* STX  */
	STY = 271,                     /* STY  */
	TAX = 272,                     /* TAX  */
	TAY = 273,                     /* TAY  */
	TXA = 274,                     /* TXA  */
	TYA = 275,                     /* TYA  */
	TSX = 276,                     /* TSX  */
	TXS = 277,                     /* TXS  */
	PHA = 278,                     /* PHA  */
	PHP = 279,                     /* PHP  */
	PLA = 280,                     /* PLA  */
	PLP = 281,                     /* PLP  */
	AND = 282,                     /* AND  */
	EOR = 283,                     /* EOR  */
	ORA = 284,                     /* ORA  */
	BIT = 285,                     /* BIT  */
	ADC = 286,                     /* ADC  */
	SBC = 287,                     /* SBC  */
	CMP = 288,                     /* CMP  */
	CPX = 289,                     /* CPX  */
	CPY = 290,                     /* CPY  */
	INC = 291,                     /* INC  */
	INX = 292,                     /* INX  */
	INY = 293,                     /* INY  */
	DEC = 294,                     /* DEC  */
	DEX = 295,                     /* DEX  */
	DEY = 296,                     /* DEY  */
	ASL = 297,                     /* ASL  */
	LSR = 298,                     /* LSR  */
	ROL = 299,                     /* ROL  */
	ROR = 300,                     /* ROR  */
	JMP = 301,                     /* JMP  */
	JSR = 302,                     /* JSR  */
	RTS = 303,                     /* RTS  */
	BCC = 304,                     /* BCC  */
	BCS = 305,                     /* BCS  */
	BEQ = 306,                     /* BEQ  */
	BMI = 307,                     /* BMI  */
	BNE = 308,                     /* BNE  */
	BPL = 309,                     /* BPL  */
	BVC = 310,                     /* BVC  */
	BVS = 311,                     /* BVS  */
	CLC = 312,                     /* CLC  */
	CLD = 313,                     /* CLD  */
	CLI = 314,                     /* CLI  */
	CLV = 315,                     /* CLV  */
	SEC = 316,                     /* SEC  */
	SED = 317,                     /* SED  */
	SEI = 318,                     /* SEI  */
	BRK = 319,                     /* BRK  */
	NOP = 320,                     /* NOP  */
	RTI = 321                      /* RTI  */
};
typedef yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
	const char* str;
	AstNode* node;
	ListNode* list;
	InstructionNode* instruction;
};
typedef YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
	int first_line;
	int first_column;
	int last_line;
	int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;
extern YYLTYPE yylloc;

int yyparse(void);

#endif /* !YY_YY_ASM6502_TAB_H_INCLUDED  */