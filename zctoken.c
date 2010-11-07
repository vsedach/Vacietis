/* -*- Mode: C; Package: (CParse C); Tab-width: 5 -*-
 * File: ZCTOKEN.C
 *
 *	This code has been placed in the public domain.
 *
 * This file contains the hand-coded tokenizer.
 * Design inspired by Jason T. Linhart.
 */

#include "ytab.h"

/* ---------------- Character type table ---------------- */
/* We use slightly different tables   #include <ctype.h> */
#define _U	0001
#define _L	0002
#define _N	0004
#define _S	0010
#define _P	0020
#define _C	0040
#define _X	0100
#define _SYM	0200			/* _ and $ */
#define _O	0400			/* octal digits */

#define isalpha(c)	((ctype+1)[c]&(_U|_L))
#define isupper(c)	((ctype+1)[c]&_U)
#define islower(c)	((ctype+1)[c]&_L)
#define isdigit(c)	((ctype+1)[c]&_N)
#define isxdigit(c)	((ctype+1)[c]&(_N|_X))
#define isodigit(c)	((ctype+1)[c]&_O)
#define isspace(c)	((ctype+1)[c]&_S)
#define ispunct(c)	((ctype+1)[c]&_P)
#define isalnum(c)	((ctype+1)[c]&(_U|_L|_N))
#define issym(c)	((ctype+1)[c]&(_U|_L|_N|_SYM))
#define isprint(c)	((ctype+1)[c]&(_P|_U|_L|_N))
#define iscntrl(c)	((ctype+1)[c]&_C)
#define isascii(c)	((unsigned)(c)<=0177)
#define toascii(c)	((c)&0177)
#define _toupper(c)	((c)-'a'+'A')
#define _tolower(c)	((c)-'A'+'a')


unsigned short ctype[257] = {
 /* -1 (EOF) */	0,
 /* 00 */    0020,   0020,   0020,   0020,
 /* 04 */    0020,   0020,   0020,   0020,
 /* 010 */   0020,   0020,   0020,   0020,
 /* 014 */   0020,   0020,   0020,   0020,
 /* 020 */   0020,   0020,   0020,   0020,
 /* 024 */   0020,   0020,   0020,   0020,
 /* 030 */   0020,   0020,   0020,   0020,
 /* 034 */   0020,   0020,   0020,   0020,
 /* ' ' */   0010,   0020,   0020,   0020,
 /* '$' */   0220,   0020,   0020,   0020,
 /* '(' */   0020,   0020,   0020,   0020,
 /* ',' */   0020,   0020,   0020,   0020,
 /* '0' */   0404,   0404,   0404,   0404,
 /* '4' */   0404,   0404,   0404,   0404,
 /* '8' */   0004,   0004,   0020,   0020,
 /* '<' */   0020,   0020,   0020,   0020,
 /* '@' */   0020,   0101,   0101,   0101,
 /* 'D' */   0101,   0101,   0101,   0001,
 /* 'H' */   0001,   0001,   0001,   0001,
 /* 'L' */   0001,   0001,   0001,   0001,
 /* 'P' */   0001,   0001,   0001,   0001,
 /* 'T' */   0001,   0001,   0001,   0001,
 /* 'X' */   0001,   0001,   0001,   0020,
 /* '\' */   0020,   0020,   0020,   0220,
 /* '`' */   0020,   0102,   0102,   0102,
 /* 'd' */   0102,   0102,   0102,   0002,
 /* 'h' */   0002,   0002,   0002,   0002,
 /* 'l' */   0002,   0002,   0002,   0002,
 /* 'p' */   0002,   0002,   0002,   0002,
 /* 't' */   0002,   0002,   0002,   0002,
 /* 'x' */   0002,   0002,   0002,   0020,
 /* '|' */   0020,   0020,   0020,   0040,
 /* 0200 */  0040,   0040,   0040,   0040,
 /* 0204 */  0040,   0040,   0040,   0040,
 /* 0210 */  0010,   0010,   0010,   0040,
 /* 0214 */  0010,   0010,   0040,   0040,
 /* 0220 */  0040,   0040,   0040,   0040,
 /* 0224 */  0040,   0040,   0040,   0040,
 /* 0230 */  0040,   0040,   0040,   0040,
 /* 0234 */  0040,   0040,   0040,   0040,
 /* identically 0040 from here on out */
 /* 0240 */  0040,   0040,   0040,   0040,   0040,   0040,   0040,   0040,
 /* 0250 */  0040,   0040,   0040,   0040,   0040,   0040,   0040,   0040,
 /* 0260 */  0040,   0040,   0040,   0040,   0040,   0040,   0040,   0040,
 /* 0270 */  0040,   0040,   0040,   0040,   0040,   0040,   0040,   0040,
 /* 0300 */  0040,   0040,   0040,   0040,   0040,   0040,   0040,   0040,
 /* 0310 */  0040,   0040,   0040,   0040,   0040,   0040,   0040,   0040,
 /* 0320 */  0040,   0040,   0040,   0040,   0040,   0040,   0040,   0040,
 /* 0330 */  0040,   0040,   0040,   0040,   0040,   0040,   0040,   0040,
 /* 0340 */  0040,   0040,   0040,   0040,   0040,   0040,   0040,   0040,
 /* 0350 */  0040,   0040,   0040,   0040,   0040,   0040,   0040,   0040,
 /* 0360 */  0040,   0040,   0040,   0040,   0040,   0040,   0040,   0040,
 /* 0370 */  0040,   0040,   0040,   0040,   0040,   0040,   0040,   0040
	};


/* Dispatch table values */
#define DALPHA		1			/* A-Z a-z _ $ */
#define DDIGIT		2			/* 0-9 */
#define DOPER		3			/* ~ ( ) $ : , ? [ ] { } ; */
#define DDOPER		4			/* ! # % & * + - < = > ^ | */
#define DQUOTE		5			/* " ' */
#define DWHITE		6			/* <Space> <Tab> <Return> <Page> */
#define DSLASH		7			/* / */
#define DDOT		8			/* . */
#define DATSIGN	9			/* @ */
#define DEOF	   127

/*  !"#$%&'()*+,-./0123456789:;<=>?
 * @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
 * `abcdefghijklmnopqrstuvwxyz{|}~
 */

/* Dispatch table: tells what kind of token this char may start */
char dispatch[256] = {
/* 00 */		DEOF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 10 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/*  !"#$%&' */	DWHITE, DDOPER, DQUOTE, DDOPER, DALPHA, DDOPER, DDOPER, DQUOTE,
/* ()*+,-./ */ DOPER, DOPER, DDOPER, DDOPER, DOPER, DDOPER, DDOT, DSLASH,
/* 01234567 */ DDIGIT, DDIGIT, DDIGIT, DDIGIT, DDIGIT, DDIGIT, DDIGIT, DDIGIT,
/* 89:;<=>? */ DDIGIT, DDIGIT, DOPER, DOPER, DDOPER, DDOPER, DDOPER, DOPER,
/* @ABCDEFG */ DATSIGN, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA,
/* HIJKLMNO */ DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA,
/* PQRSTUVW */ DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA,
/* XYZ[\]^_ */ DALPHA, DALPHA, DALPHA, DOPER, 0, DOPER, DDOPER, DALPHA,
/* `abcdefg */ 0, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA,
/* hijklmno */ DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA,
/* pqrstuvw */ DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA, DALPHA,
/* xyz{|}~‡ */ DALPHA, DALPHA, DALPHA, DOPER, DDOPER, DOPER, DOPER, 0,
/* 80 */		0, 0, 0, 0, 0, 0, 0, 0, 0, DWHITE, 0, 0, DWHITE, DWHITE, 0, 0,
/* 90 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* A0 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* B0 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* C0 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* D0 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* E0 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* F0 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };


/* Single-char table: values are lexemes (from ytab.h) */
unsigned short single_lex[128] = {
/* 00 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 10 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/*  !"#$%&' */ 0, NOT, 0, MACTOKSTR, 0, DIVMOD, AND_ADDRESS, 0,
/* ()*+,-./ */ LPAREN, RPAREN, MUL_PTR, PLUSMINUS, COMMA, PLUSMINUS, ELEMENT, DIVMOD,
/* 01234567 */ 0, 0, 0, 0, 0, 0, 0, 0, 
/* 89:;<=>? */ 0, 0, COLON, SEMI, COMPARISON, ASSIGN, COMPARISON, QMARK,
/* @ABCDEFG */ 0, 0, 0, 0, 0, 0, 0, 0,
/* HIJKLMNO */ 0, 0, 0, 0, 0, 0, 0, 0,
/* PQRSTUVW */ 0, 0, 0, 0, 0, 0, 0, 0,
/* XYZ[\]^_ */ 0, 0, 0, LBRACKET, 0, RBRACKET, BIT_XOR, 0,
/* `abcdefg */ 0, 0, 0, 0, 0, 0, 0, 0,
/* hijklmno */ 0, 0, 0, 0, 0, 0, 0, 0,
/* pqrstuvw */ 0, 0, 0, 0, 0, 0, 0, 0,
/* xyz{|}~  */ 0, 0, 0, LBRACE, BIT_OR, RBRACE, BIT_NOT, 0 };

/* Single-char yylvals */
lispval single_lval[128] = {
/* 00 */		@'(), @'(), @'(), @'(), @'(), @'(), @'(), @'(),
/* 08 */		@'(), @'(), @'(), @'(), @'(), @'(), @'(), @'(),
/* 10 */		@'(), @'(), @'(), @'(), @'(), @'(), @'(), @'(),
/* 18 */		@'(), @'(), @'(), @'(), @'(), @'(), @'(), @'(),
/*  !"#$%&' */	@'(), @'c:!, @'(), @'(), @'(), @'c:%, @'c:&, @'(),
/* ()*+,-./ */ @'(), @'(), @'c:*, @'c:+, @'(), @'c:-, @'c:\., @'c:/,
/* 01234567 */ @'(), @'(), @'(), @'(), @'(), @'(), @'(), @'(),
/* 89:;<=>? */ @'(), @'(), @'(), @'(), @'c:<, @'c:=, @'c:>, @'(),
/* @ABCDEFG */ @'(), @'(), @'(), @'(), @'(), @'(), @'(), @'(),
/* HIJKLMNO */ @'(), @'(), @'(), @'(), @'(), @'(), @'(), @'(),
/* PQRSTUVW */ @'(), @'(), @'(), @'(), @'(), @'(), @'(), @'(),
/* XYZ[\]^_ */ @'(), @'(), @'(), @'(), @'(), @'(), @'c:^, @'(),
/* `abcdefg */ @'(), @'(), @'(), @'(), @'(), @'(), @'(), @'(),
/* hijklmno */ @'(), @'(), @'(), @'(), @'(), @'(), @'(), @'(),
/* pqrstuvw */ @'(), @'(), @'(), @'(), @'(), @'(), @'(), @'(),
/* xyz{|}~  */ @'(), @'(), @'(), @'(), @'c:\|, @'(), @'c:~, @'() };


/* Double-char table: values are lexemes (from ytab.h) */
unsigned short double_lex[128] = {
/* 00 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 10 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/*  !"#$%&' */ 0, 0, 0, MACTOKCAT, 0, 0, AND, 0,
/* ()*+,-./ */ 0, 0, 0, INCREMENT, 0, INCREMENT, 0, 0,
/* 01234567 */ 0, 0, 0, 0, 0, 0, 0, 0, 
/* 89:;<=>? */ 0, 0, 0, 0, SHIFT, EQUALITY, SHIFT, 0,
/* @ABCDEFG */ 0, 0, 0, 0, 0, 0, 0, 0,
/* HIJKLMNO */ 0, 0, 0, 0, 0, 0, 0, 0,
/* PQRSTUVW */ 0, 0, 0, 0, 0, 0, 0, 0,
/* XYZ[\]^_ */ 0, 0, 0, 0, 0, 0, 0, 0,
/* `abcdefg */ 0, 0, 0, 0, 0, 0, 0, 0,
/* hijklmno */ 0, 0, 0, 0, 0, 0, 0, 0,
/* pqrstuvw */ 0, 0, 0, 0, 0, 0, 0, 0,
/* xyz{|}~  */ 0, 0, 0, 0, OR, 0, 0, 0 };

/* Double-char yylvals */
lispval double_lval[128] = {
/* 00 */		@(), @(), @(), @(), @(), @(), @(), @(),
/* 08 */		@(), @(), @(), @(), @(), @(), @(), @(),
/* 10 */		@(), @(), @(), @(), @(), @(), @(), @(),
/* 18 */		@(), @(), @(), @(), @(), @(), @(), @(),
/*  !"#$%&' */	@(), @(), @(), @(), @(), @(), @'c:&&, @(),
/* ()*+,-./ */ @(), @(), @(), @'c:++, @(), @'c:--, @(), @(),
/* 01234567 */ @(), @(), @(), @(), @(), @(), @(), @(),
/* 89:;<=>? */ @(), @(), @(), @(), @'c:<<, @'c:==, @'c:>>, @(),
/* @ABCDEFG */ @(), @(), @(), @(), @(), @(), @(), @(),
/* HIJKLMNO */ @(), @(), @(), @(), @(), @(), @(), @(),
/* PQRSTUVW */ @(), @(), @(), @(), @(), @(), @(), @(),
/* XYZ[\]^_ */ @(), @(), @(), @(), @(), @(), @(), @(),
/* `abcdefg */ @(), @(), @(), @(), @(), @(), @(), @(),
/* hijklmno */ @(), @(), @(), @(), @(), @(), @(), @(),
/* pqrstuvw */ @(), @(), @(), @(), @(), @(), @(), @(),
/* xyz{|}~  */ @(), @(), @(), @(), @'c:\|\|, @(), @(), @() };


/* Assign table (char=): values are lexemes */
unsigned short assign_lex[128] = {
/* 00 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 10 */		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/*  !"#$%&' */ 0, EQUALITY, 0, 0, 0, OP_ASSIGN, OP_ASSIGN, 0,
/* ()*+,-./ */ 0, 0, OP_ASSIGN, OP_ASSIGN, 0, OP_ASSIGN, 0, OP_ASSIGN,
/* 01234567 */ 0, 0, 0, 0, 0, 0, 0, 0, 
/* 89:;<=>? */ 0, 0, 0, 0, COMPARISON, 0, COMPARISON, 0,
/* @ABCDEFG */ 0, 0, 0, 0, 0, 0, 0, 0,
/* HIJKLMNO */ 0, 0, 0, 0, 0, 0, 0, 0,
/* PQRSTUVW */ 0, 0, 0, 0, 0, 0, 0, 0,
/* XYZ[\]^_ */ 0, 0, 0, 0, 0, 0, OP_ASSIGN, 0,
/* `abcdefg */ 0, 0, 0, 0, 0, 0, 0, 0,
/* hijklmno */ 0, 0, 0, 0, 0, 0, 0, 0,
/* pqrstuvw */ 0, 0, 0, 0, 0, 0, 0, 0,
/* xyz{|}~  */ 0, 0, 0, 0, OP_ASSIGN, 0, 0, 0 };

/* Assign yylvals */
lispval assign_lval[128] = {
/* 00 */		@(), @(), @(), @(), @(), @(), @(), @(),
/* 08 */		@(), @(), @(), @(), @(), @(), @(), @(),
/* 10 */		@(), @(), @(), @(), @(), @(), @(), @(),
/* 18 */		@(), @(), @(), @(), @(), @(), @(), @(),
/*  !"#$%&' */	@(), @'c:!=, @(), @(), @(), @'c:%=, @'c:&=, @(),
/* ()*+,-./ */ @(), @(), @'c:*=, @'c:+=, @(), @'c:-=, @(), @'c:/=,
/* 01234567 */ @(), @(), @(), @(), @(), @(), @(), @(),
/* 89:;<=>? */ @(), @(), @(), @(), @'c:<=, @(), @'c:>=, @(),
/* @ABCDEFG */ @(), @(), @(), @(), @(), @(), @(), @(),
/* HIJKLMNO */ @(), @(), @(), @(), @(), @(), @(), @(),
/* PQRSTUVW */ @(), @(), @(), @(), @(), @(), @(), @(),
/* XYZ[\]^_ */ @(), @(), @(), @(), @(), @(), @'c:^=, @(),
/* `abcdefg */ @(), @(), @(), @(), @(), @(), @(), @(),
/* hijklmno */ @(), @(), @(), @(), @(), @(), @(), @(),
/* pqrstuvw */ @(), @(), @(), @(), @(), @(), @(), @(),
/* xyz{|}~  */ @(), @(), @(), @(), @'c:\|=, @(), @(), @() };


#define EOF			0			/* Note: not -1 */
#define NUL			'\0'
#define TRUE			((char) 1)
#define FALSE			((char) 0)
#define NULL			((char *) 0)
typedef char FLAG;
#define ENDIF			16384

#define TOKBUFSIZE		4096
#define SAVBUFSIZE		6144

/* Easy access to Lisp primitives */
#define car(l)			GL$CAR(l)
#define cdr(l)			GL$CDR(l)
#define cadr(l)		GL$CADR(l)
#define cdar(l)		GL$CDAR(l)
#define caar(l)		GL$CAAR(l)
#define cddr(l)		GL$CDDR(l)
#define cadar(l)		GL$CADAR(l)
#define caddr(l)		GL$CADDR(l)
#define cdddr(l)		GL$CDDDR(l)
#define cadddr(l)		GL$CADDDR(l)
#define push(x,l)		GL$PUSH(x,l)
#define pop(l)			GL$POP(l)
#define cons(a,d)		GL$CONS(a,d)
#define rplaca(l,x)		GL$RPLACA(l,x)
#define rplacd(l,x)		GL$RPLACD(l,x)
#define nreverse(l)		GL$NREVERSE(l)
#define list			GL$LIST


/* This struct holds all the internal variables of the lexer (for re-entrancy). */
struct lexerstate {
	char curchar;				/* current input character */
	char *bufptr;				/* where to get next character */
	lispval yylval;			/* token being returned */
	lispval (*cpstream)();		/* c-parser stream we're reading from */
	char *tokbuf;				/* current token buffer */
	char *savbuf, *savbufp;		/* text-save buffer with end pointer */
	char *lastbufptr;			/* value of bufptr when text-saving started */
	lispval ifstack;			/* stack of active #if*s */
	int ifstate;				/* see below */
	int macrostate;			/* see below */
	lispval macro,				/* macro being expanded */
		   macparams,				/* parameters thereof */
		   macargs;				/* arguments thereto */
	FLAG macargstr,				/* is next macro arg to be stringified? */
		mactokcat;				/* is next macro token 2B concatenated? */
	char *macexpstart;				/* bufptr to beginning of macro exp */
	};

/* Possible values for ifstate in the above: */
#define IFNONE		0			/* no #if in progress */
#define IFSTART	1			/* #if (or #elif) encountered */
#define IFPARSE	2			/* #if's expression being parsed */
#define IFDEFINED	3			/* in scope of 'defined' operator */

/* Possible values for macrostate in the above: */
#define MNONE		0			/* no macro processing going on */
#define MDEFINING	1			/* macro definition being read */
#define MARGGING	2			/* macro args (or params) being gathered */
#define MEXPANDING	3			/* macro being expanded */


/* Routines in this file (in order) (indented so as not to fire sectionizer) */
  struct lexerstate *LNew();
  void LReset();
  lispval Lyyval();
  char LTyi();
  int LCurLine();
  FLAG LCommentsNest();
  int LToken();
  FLAG LComment();
  int LSymbol(), LSymbol1();
  lispval LInt(), LFloat(), LString(), LChar();
  int LPreProc();
  void LDefine(), LUndef(), LIfdef(), LElse(), LEndif(), LInclude();
  int LLisp();
  lispval LCondP(), LCondCheck(), LGetSym();
  void LSkipToEOL(), LMacArgs(), LMacExpand(), LMacroSymbol();
  lispval LMacArg();
  void LScanExpansion(), LSaveText();
  char LNextLine();
  void LSetBuf();
  int LBufIndex();

/* The following are copied from ZCCLIB.C, to reduce compilation dependencies. */
  static char *strcpy(), *memcpy();
  static int strlen(), strcmp(), strncmp();
  static lispval str2lisp();
  static int toupper(), tolower();


/* Creates a new instance of the lexer and returns it. */
struct lexerstate *
LNew ()
{
	struct lexerstate *lstate;
	char *tokbuf;
	int tokbufsize = TOKBUFSIZE;	/* for Lisp inclusion */

	lstate = (struct lexerstate *) calloc (1, sizeof (struct lexerstate));
	lstate->tokbuf = tokbuf = (char *) calloc (TOKBUFSIZE, 1);
#lisp
	(gl:setf (zeta-c:zcprim>array-freelist-link |tokbuf.array|)
		    (gl:make-array |tokbufsize| :type gl:art-string :fill-pointer 0
					    :displaced-to |tokbuf.array|
					    :displaced-index-offset 0))
#endlisp
	lstate->savbuf = (char *) calloc (SAVBUFSIZE, 1);
	/* Only the array part of the returned pointer is kept -- index must be 0! */
	return lstate;
}


/* Returns a token in a form Lisp will like (i.e. as an ART-STRING).
 * tokstart, tokend delimit the token; both must point into lstate->tokbuf
 * as created by LNew.  Note that only one of these can be in use at a time
 * for any given instance of the lexer. */
lispval
LLispTok (tokstart, tokend)
	char *tokstart, *tokend;
{
	lispval lisptok;
	int length = tokend - tokstart;

	lisptok = @(zeta-c:zcprim>array-freelist-link |tokstart.array|);
#lisp
	(gl:setf #+3600 (si:array-index-offset-field |lisptok|)
		    #-3600 (gl:%p-contents-offset |lisptok| 3)
		    |tokstart.index|)
	(gl:setf (gl:fill-pointer |lisptok|) |length|)
#endlisp
	return lisptok;
}


/* Initializes a lexer instance. */
LInit (lstate, cpstream)
	struct lexerstate *lstate;
	lispval (*cpstream)();
{
	lstate->bufptr = lstate->savbufp = NULL;
	lstate->cpstream = cpstream;
	lstate->curchar = NUL;
	lstate->ifstack = @GL:NIL;
	lstate->ifstate = IFNONE;
	lstate->macrostate = MNONE;
	lstate->macro = @GL:NIL;
}


/* Resets the state of the lexer.  This is used for interactive input,
 * in which aborts and the like can leave the state inconsistent. */
void
LReset (lstate)
	struct lexerstate *lstate;
{
	lstate->curchar = NUL;
	lstate->bufptr = lstate->savbufp = NULL;
	lstate->ifstack = @GL:NIL;
	lstate->ifstate = IFNONE;
	lstate->macrostate = MNONE;
	lstate->macro = @GL:NIL;
}


/* Returns lstate->yylval (to be called from outside). */
lispval
Lyylval (lstate)
	struct lexerstate *lstate;
{
	return lstate->yylval;
}


/* Macros for dealing with the input buffering scheme.
 * They assume the following local declarations:
 *	struct lexerstate *lstate;
 *	char c, *cptr;
 *	char *token = lstate->tokbuf;   (only if NEXT is called)
 */
/* LOADSTATE sets things up on function entry, reading the first char. 
 * lstate->bufptr == NULL case happens only before first char of a file is read.
 * lstate->curchar == NUL case happens only after a HALFSKIP or at EOF.  */
#define LOADSTATE	( c = (cptr = lstate->bufptr) ? ((c = lstate->curchar) ? c : LNextLine (lstate, &cptr)) : (cptr = "", '\n') )
/* SAVESTATE puts things back before returning.  Be sure you call it on every
 * path out of the function!  Note, if you SAVESTATE but don't return, you don't
 * have to LOADSTATE; just continue. */
#define SAVESTATE	( lstate->curchar = c,  lstate->bufptr = cptr )
/* SKIP reads the next character into "c". */
#define SKIP		( (c = *cptr++) ? c : (c = LNextLine (lstate, &cptr)) )
/* HALFSKIP advances the input pointer without actually forcing the next char
 * to be read.  Use it only right before a SAVESTATE. */
#define HALFSKIP	( c = *cptr++ )
/* NEXT appends the current char to token, then reads the next char. */
#define NEXT		( *token++ = c, SKIP )


/* Reads a character from the input.  Everything else in this file
 * uses the macros above, but this is available for calling from
 * outside. */
char
LTyi (lstate)
	struct lexerstate *lstate;
{
	char c, *cptr, t;

	LOADSTATE;
	t = c;
	SKIP;
	SAVESTATE;
	return t;
}


/* "Unreads" a character.  This is only to be called from outside. */
void
LUnTyi (lstate, unc)
	struct lexerstate *lstate;
	char unc;
{
	char c, *cptr;

	LOADSTATE;
	--cptr;
	c = unc;
/*	if (c != cptr[-1])
		LError (lstate->cpstream, 0,
			   @"Internal error: char being untyi-ed doesn't match buffer");*/
	SAVESTATE;
}


/* What file are we reading from?  (Returns a pathname object.) */
lispval
LCurFile (lstate)
	struct lexerstate *lstate;
{
	return lstate->cpstream (@:current-file);
}


/* What line are we reading on? */
int
LCurLine (lstate)
	struct lexerstate *lstate;
{
	return (int) (lstate->cpstream (@:current-line));
}


/* Do comments nest here? */
FLAG
LCommentsNest (lstate)
	struct lexerstate *lstate;
{
	return (lstate->cpstream (@:comments-nest-p) ? TRUE : FALSE);
}


/* Are we at the end of the current bufferful of input?
   (Used in interactive mode only.) */
FLAG
LAtEnd (lstate)
	struct lexerstate *lstate;
{
	return lstate->curchar == NUL;
}


/* Reads the next token.  Returns the lexeme for yyparse;
 * sets lstate->yylval to the token object.
 * `skipping', if TRUE, says that we're reading inside the scope of a failed
 * #conditional; we have to tokenize but don't need the tokens.
 */
int
LToken (lstate)
	struct lexerstate *lstate;
{
	char c, *cptr, *token, t, ch, *tsavbufp, *tcptr;
	int l, i, GL$LENGTH();
	FLAG painted, floatp, hexp, shortp, longp, unsignedp;
	lispval skipping;			/* fast boolean */
	lispval LFloat(), LInt(), LString(), LChar();

	if (lstate->ifstate == IFSTART) {		   /* a #if or #elif encountered */
		lstate->ifstate = IFPARSE;
		return SHARPIF;
		}
	LOADSTATE;
	for (;;) {
	  floatp = FALSE;				  /* slighly bogus indentation, sorry */
	  token = lstate->tokbuf;
	  switch (dispatch[c]) {
	
	  case DEOF:						 /* end of "file" (input source) */
		if (lstate->macrostate != MNONE) {
			SAVESTATE;
			return ENDOFSTREAM;
		}
		/* If there are any active #if*s, warn user */
		for (; lstate->ifstack && caddr(car(lstate->ifstack)) == LCurFile (lstate);
			lstate->ifstack = cdr(lstate->ifstack))
			LWarnNoCtx (@"Missing #endif to ~A on line ~A",
					  car(cdar(lstate->ifstack)),
					  cadr(cdar(lstate->ifstack)));
		/* If there's no input left after popping, return. */
		if (!lstate->cpstream (@:pop-input-source)) {
			SAVESTATE;
			return ENDOFSTREAM;
		}
		LOADSTATE;			/* will load new stuff since source popped */
		break;
	  case DWHITE:
		if (c == '\n'  &&  lstate->macrostate == MDEFINING) {
			SAVESTATE;
			return ENDOFSTREAM;
		}
		if (c == '\n'  &&  lstate->ifstate == IFPARSE) {
			SAVESTATE;
			return SHARPIF;
		}
		t = c;
		SKIP;
		if (t == '\n') {
			lstate->macexpstart = cptr - 1;	  /* previous line now lost */
			if (c == '#') {
				SKIP; SAVESTATE;
				if (l = LPreProc (lstate)) return l;   /* #lisp uses this */
				skipping = !LCondP (lstate);				    /* cache */
				LOADSTATE;
			}
		}
		else {
			SAVESTATE;
			LSaveText (lstate);
			tsavbufp = lstate->savbufp;			 /* condense grayspace */
			while (c == ' '  ||  c == '\t') SKIP;
			if (tsavbufp) {
				if (lstate->macrostate == MEXPANDING  &&  lstate->mactokcat)
					--tsavbufp;
				else tsavbufp[-1] = ' ';
				lstate->savbufp = tsavbufp;
				lstate->lastbufptr = cptr - 1;
			}
		}
		break;
	  case DALPHA:									    /* identifier */
		tcptr = cptr - 1;		    /* save place for macro-exp code below */
		while (issym(NEXT));
		*token = NUL;
		if (c == '\5') {					     /* is token "painted"? */
			SKIP;
			painted = TRUE;
			SAVESTATE;
			LSaveText (lstate);			/* delete paint from save buffer */
			--lstate->savbufp;				      /* works even if null */
		}
		else painted = FALSE;
		if (skipping) break;
		SAVESTATE;
		if (lstate->macrostate == MNONE) {	  /* unless processing macro ... */
			if (l = LSymbol (lstate, token, painted)) return l;
			/* else symbol is a macro name to be expanded */
			lstate->macexpstart = tcptr;
			LMacExpand (lstate);
			LOADSTATE;
		}
		else if (lstate->macrostate == MDEFINING) {
			LSymbol (lstate, token, TRUE);	    /* get name into yylval */
			if (lstate->macro == lstate->yylval)	 /* if token is macro, */
				*lstate->savbufp++ = '\5';			    /* "paint" it */
		}
		else if (lstate->macrostate == MEXPANDING) {
			LMacroSymbol (lstate);
			LOADSTATE;
		}
		else return SYMBOL;
		break;
	  case DDOPER:
		/* These chars can be either doubled or followed by =, or maybe both */
		t = c;
		SKIP;
		if (c == t  &&  (l = double_lex[t])) {					  /* XX */
			SKIP;
			if (l == SHIFT  &&  c == '=') {			    /* <<= or >>= */
				SKIP;
				if (skipping) break;
				SAVESTATE;
				lstate->yylval = (t == '<') ? @'c:<<= : @'c:>>= ;
				return OP_ASSIGN;
				}
			else {
				if (skipping) break;
				SAVESTATE;
				lstate->yylval = double_lval[t];
				return l;
			}
		}
		else if (c == '='  &&  (l = assign_lex[t])) {			  /* X= */
			SKIP;
			if (skipping) break;
			SAVESTATE;
			lstate->yylval = assign_lval[t];
			return l;
		}
		else if (t == '-'  &&  c == '>') {						  /* -> */
			SKIP;
			if (skipping) break;
			SAVESTATE;
			lstate->yylval = @'c:-> ;
			return ELEMENT;
		}
		else {											   /* X */
			if (skipping) break;
			SAVESTATE;
			lstate->yylval = single_lval[t];
			return single_lex[t];
		}
		break;
	  case DOPER:
		t = c;
		if (skipping) {
			SKIP;
			break;
		}
		HALFSKIP;		    /* for C listener: don't force read of next char */
		SAVESTATE;			    /* this one might have been ';' or '}' */
		lstate->yylval = single_lval[t];
		return single_lex[t];
	  case DSLASH:
		SAVESTATE;
		LSaveText (lstate);
		tsavbufp = lstate->savbufp;
		SKIP;
		if (c != '*') {
			if (c == '=') {
				SKIP;
				if (skipping) break;
				SAVESTATE;
				lstate->yylval = @'c:/= ;
				return OP_ASSIGN;
			}
			if (skipping) break;
			SAVESTATE;
			lstate->yylval = @'c:/ ;
			return DIVMOD;
		}
		SKIP;
		SAVESTATE;
		{	int nullp = LComment (lstate);
			if (tsavbufp) {					 /* comment => #\Space */
				*tsavbufp++ = ' ';
				if (nullp  &&  lstate->macrostate == MDEFINING) {
					strcpy (tsavbufp, "## ");	   /* null comment was */
					tsavbufp += 3;			  /* old-style token concat */
				}
				lstate->savbufp = tsavbufp;
				lstate->lastbufptr = lstate->bufptr - 1;
			}
		}
		LOADSTATE;
		break;
	  case DDOT:
		if (NEXT == '.') {
			if (SKIP != '.') {
				SAVESTATE;
				LError (lstate->cpstream, -1, @"Illegal token `..'");
				}
			if (skipping) break;
			SAVESTATE;
			lstate->yylval = @'c:|...|;
			return ELLIPSIS;
		}
		else if (!isdigit(c)) {
			if (skipping) break;
			SAVESTATE;
			lstate->yylval = @'c:\.;
			return ELEMENT;
		}
		floatp = TRUE;			/* and fall thru */
	  case DDIGIT:
		hexp = shortp = longp = unsignedp = FALSE;
		if (!floatp  &&  c == '0'  &&  toupper(NEXT) == 'X') {
			SKIP;
			hexp = TRUE;
			while (isxdigit(NEXT));
		}
		else while (isdigit(c)) NEXT;
		if (!hexp  &&  c == '.') {
			floatp = TRUE;
			while (isdigit(NEXT));
		}
		if (!hexp  &&  toupper(c) == 'E') {
			floatp = TRUE;
			NEXT;
			if (c == '+'  ||  c == '-') NEXT;
			while (isdigit(NEXT));
		}
		if (floatp) {
			if (toupper(c) == 'F') { SKIP; shortp = TRUE; }
			else if (toupper(c) == 'L') { SKIP; longp = TRUE; }
		}
		else for (;;) {
			if (toupper(c) == 'U') { SKIP; unsignedp = TRUE; }
			else if (toupper(c) == 'L') { SKIP; longp = TRUE; }
			else break;
		}
		/* Only now do we actually do the conversions */
		if (skipping) break;
		SAVESTATE;
		lstate->yylval = floatp 
					  ? LFloat (lstate->tokbuf, token, shortp, longp)
					  : LInt (lstate, lstate->tokbuf, token, hexp, unsignedp, longp);
		return NUMBER;
	  case DQUOTE:
		t = c;
		SKIP;
		while (c != t) {
			if (c == '\\') {
				SKIP;
				if (isodigit(c)) {
					for (i = 0, ch = 0; i < 3  &&  isodigit(c); ++i, SKIP)
						ch = 8*ch + c - '0';
					*token++ = ch;
				}
				else if (c == 'x') {
					for (i = 0, ch = 0; i < 2  &&  isxdigit(c); ++i, SKIP)
						ch = 16*ch + (c <= '9') ? c - '0'
										    : (c | 0x20) - 'a' + 10;
					*token++ = ch;
				}
				else {
					switch (c) {
						case 'r': *token++ = '\r'; break;
						case 'n': *token++ = '\n'; break;
						case 't': *token++ = '\t'; break;
						case 'b': *token++ = '\b'; break;
						case 'f': *token++ = '\f'; break;
						case 'v': *token++ = '\v'; break;
						default: *token++ = c; break;
					}
					SKIP;
				}
			}
			else if (c == '\n') {
				SAVESTATE;
				LError (lstate->cpstream, 0, @"Missing closing ~C", t);
				break;
			}
			else NEXT;
		}
		if (c != '\n') SKIP;
		if (skipping) break;
		SAVESTATE;
		if (t == '"') lstate->yylval = LString (lstate->tokbuf, token);
		else lstate->yylval = LChar (lstate, lstate->tokbuf, token);
		return t == '"' ? STRING : CHARCONST;
	  case DATSIGN:			/* Lisp form inclusion */
		SKIP;
		SAVESTATE;
		lstate->yylval = list(@'c:\#LISP, @'c:((|lispval|)),
						  lstate->cpstream (@:readlisp));
		LOADSTATE;
		if (!skipping) return SYMBOL;
		break;
	  default:
	  	SAVESTATE;
		LError (lstate->cpstream, 0, @"Illegal character '~C'", c);
		break;
	  }
	}
}


/* Finds the end of a comment; returns TRUE if the comment was null.
 * Expects the initial '/' and '*' to be already read.
 */
FLAG
LComment(lstate)
	struct lexerstate *lstate;
{
	char *cptr, oc;
	int c, nchars, startline, commentsnest;

	LOADSTATE;
	startline = LCurLine (lstate);
	commentsnest = LCommentsNest (lstate);
	oc = NUL;
	nchars = -1;
	while (oc != '*'  ||  c != '/') {
		if (c == EOF) {
			SAVESTATE;
			LError (lstate->cpstream, 0, @"Input stream ended in the middle of a comment, which started on line ~D", startline);
		}
		if (commentsnest  &&  oc == '/'  &&  c == '*') {
			SAVESTATE;
			LComment(lstate);
			LOADSTATE;
		}
		oc = c;
		SKIP;
		++nchars;
	}
	SKIP;
	SAVESTATE;
	return nchars == 0;
}


/* Converts a symbol.  Returns its lexeme, or 0 for a macro reference. */
int
LSymbol (lstate, tokend, nomacro)
	struct lexerstate *lstate;
	char *tokend;
	FLAG nomacro;
{
	return (int) lstate->cpstream(@:process-symbol, lstate->ifstate, nomacro,
							LLispTok (lstate->tokbuf, tokend));
}


/* Called by (:method c-parser :process-symbol) (called above). 
 * lexeme 0 means this symbol is a macro reference. */
int
LSymbol1 (lstate, lexeme, symbol, macparams, macdef)
	struct lexerstate *lstate;
	lispval symbol;
	int lexeme;
	optarg lispval macparams, macdef;
{
	if (lexeme) lstate->yylval = symbol;
	else {
		lstate->macro = symbol;
		lstate->macparams = macparams;
		lstate->yylval = macdef;
	}
	if (lexeme == DEFINED) lstate->ifstate = IFDEFINED;
	else if (lstate->ifstate == IFDEFINED) lstate->ifstate = IFPARSE;
	return lexeme;
}


/* Converts an integer token; hexp, unsignedp, and longp tell if there was
 * a leading "0x", a trailing "u", and/or a trailing "l"; these have been
 * deleted from the token string. */
lispval
LInt (lstate, tokstart, tokend, hexp, unsignedp, longp)
	struct lexerstate *lstate;
	char *tokstart, *tokend;
	FLAG hexp, unsignedp, longp;
{
	int toklen = tokend - tokstart;
	lispval type = unsignedp ? (longp ? @(zeta-c:zctype>unsigned-long)
							    : @(zeta-c:zctype>unsigned))
						: (longp ? @(zeta-c:zctype>long) : @GL:NIL);
	char *ttok;
	lispval lisptok, value;
	int base = hexp ? 16 : *tokstart == '0' ? 8 : 10;

	if (base == 8) for (ttok = tokstart; ttok < tokend; ++ttok)
		if (*ttok > '7')
			LError (lstate->cpstream, ttok - tokend,
				   @"Digit '~C' not allowed in octal number.", *ttok);
	lisptok = LLispTok (tokstart, tokend);
	value = @(si:xr-read-fixnum-internal |lisptok| 0 (gl:string-length |lisptok|)
								  |base|);
	return type ? @`(c:quote+ ,|value| ,|type|) : value;
}


/* Converts a floating-point token; shortp and longp tell if there was a
 * trailing "f" or "l" (but in fact "l" is ignored, as ZETA-C does not have
 * a separate long double type); these have been deleted from the token string. */
lispval
LFloat (tokstart, tokend, shortp, longp)
	char *tokstart, *tokend;
	FLAG shortp, longp;
{
	lispval lisptok = LLispTok (tokstart, tokend);

	GL$IGNORE(longp);				/* long double = double */
	if (shortp) return @(si:xr-read-flonum |lisptok| #+3600 :single-float
										    #-3600 :small-flonum);
	else return @(si:xr-read-flonum |lisptok| #+3600 :double-float
									  #-3600 gl:nil);
}


/* Converts a string.  The quotes have been deleted, and escape sequences
 * processed. */
lispval
LString (tokstart, tokend)
	char *tokstart, *tokend;
{
	int toklen = tokend - tokstart, i, c;
	lispval string;

	string = @(gl:make-array |toklen| :type gl:art-string);
	for (i = 0; i < toklen; ++i) {
		c = tokstart[i];
		@(gl:aset (zeta-c:code-char |c|) |string| |i|);
		}
	return @`(c:string+ ,|string|);
}


/* Converts a character constant.  The single quotes have been deleted, and escape
 * sequences processed.  ZETA-C supports multicharacter constants ala 4.2bsd: the
 * first character goes in the high-order position (backwards from a string).
 * There is no length limit (!). */
lispval
LChar (lstate, tokstart, tokend)
	struct lexerstate *lstate;
	char *tokstart, *tokend;
{
	int toklen = tokend - tokstart;
	int value = 0, i;
	lispval type = toklen < 2 ? @(zeta-c:zctype>char)
						 : toklen == 2 ? @(zeta-c:zctype>unsigned-short)
									: toklen <= 4
									  ? @(zeta-c:zctype>unsigned)
									  : @(zeta-c:zctype>unsigned-long);

	if (toklen < 1)
		LError (lstate->cpstream, 0,
			   @"Char constant must have at least one char");
	/* 4bsd-style multicharacter constants.  These are "backwards" relative
	 * to strings: first character goes in high-order spot. */
	for (i = 0; i < toklen; ++i) value = (value << 8) + tokstart[i];
	return @`(c:quote+ ,|value| ,|type|);
}


/* Used often by preprocessor routines */
#define SKIPGRAY	while (c == ' '  ||  c == '\t') SKIP;  /* skip grayspace */
#define ENDCHECK	{ if (c == '\n') { SAVESTATE; LEOLErr(lstate); } else if (c == 0) { SAVESTATE; LEOFErr(lstate); } }


LEOLErr (lstate)
	struct lexerstate *lstate;
{
	LError (lstate->cpstream, 0, @"Unexpected end of line");
}


LEOFErr (lstate)
	struct lexerstate *lstate;
{
	LError (lstate->cpstream, 0, @"Unexpected end of file");
}


/* Does a preprocessor command
 * # has been found in col. 0
 * exits with input pointer at end of line
 * returns 0 unless LLisp is returning a lexeme
 */
int
LPreProc (lstate)
	struct lexerstate *lstate;
{
	char c, *cptr, *token = lstate->tokbuf;

	LOADSTATE;
	SKIPGRAY;
	if (c == '\n') {			/* '#' alone on a line is ignored. */
		SAVESTATE;
		return 0;
	}
	while (!isspace(c)) NEXT;	/* get command */
	*token = NUL;
	SAVESTATE;
	token = lstate->tokbuf;
	if (!strcmp (token, "define")) LDefine (lstate);
	else if (!strcmp (token, "undef")) LUndef (lstate);
	else if (!strcmp (token, "ifdef")) LIfdef (lstate, TRUE);
	else if (!strcmp (token, "ifndef")) LIfdef (lstate, FALSE);
	else if (!strcmp (token, "if")) LIf (lstate);
	else if (!strcmp (token, "elif")) LElif (lstate);
	else if (!strcmp (token, "else")) LElse (lstate);
	else if (!strcmp (token, "endif")) LEndif (lstate);
	else if (!strcmp (token, "include")) LInclude (lstate);
	else if (!strcmp (token, "line")) LLine (lstate);
	else if (!strcmp (token, "lisp")) return LLisp (lstate);
	else LError (lstate->cpstream, 0, @"Unknown preprocessor directive \"~A\"",
			   str2lisp (token));
	return 0;
}


void
LDefine (lstate)
	struct lexerstate *lstate;
{
	char c, *cptr, *savbufp, *tokend;
	lispval name, params, defn;

	if (LCondCheck (lstate)) return;
	name = LGetSym (lstate);
	LOADSTATE;
	if (c == '(') {
		SAVESTATE;
		LMacArgs (lstate);
		LOADSTATE;
		params = lstate->macargs;
	}
	else params = @'zeta-c:no-params;
	SKIPGRAY;
	SAVESTATE;
	lstate->lastbufptr = lstate->bufptr - 1;	    /* initiate text saving */
	lstate->savbufp = lstate->savbuf;
	*lstate->savbufp++ = ' ';						 /* leading space */
	lstate->macrostate = MDEFINING;
	lstate->macro = name;
	while (LToken (lstate) != ENDOFSTREAM);
	if (lstate->curchar == 0) LEOFErr (lstate);
	LSaveText (lstate);
	savbufp = lstate->savbufp;
	lstate->savbufp = lstate->lastbufptr = NULL;			    /* saving off */
	*savbufp++ = ' ';								/* trailing space */
	*savbufp++ = NUL;				   /* Need a NUL on end of definition */
	strcpy (lstate->tokbuf, lstate->savbuf);	 /* slightly roundabout but */
	tokend = lstate->tokbuf + (savbufp - lstate->savbuf);   /* needed to get */
	defn = LLispTok (lstate->tokbuf, tokend);	   /* an ART-STRING version */
	defn = @(gl:string-append |defn|);
	lstate->cpstream (@:define, name, params, defn);
	lstate->macrostate = MNONE;
}


void
LUndef (lstate)
	struct lexerstate *lstate;
{
	lispval name;

	if (LCondCheck (lstate)) return;
	name = LGetSym (lstate);
	lstate->cpstream (@:undef, name);
	LSkipToEOL (lstate);
}


/* An ifdesc, which is an element of lstate->ifstack, has the following structure:
 * (<state> <name> <file> <start-line> <else>)
 * where:
 *	<state> is: T if conditional was successful (and so were all outer ones)
 *			  :FAILED if conditional was tested and failed
 * 			  NIL if an outer conditional failed, or if a previous one
 *			    succeeded and then we hit a #else or #elif
 *	<name> is |#if|, |#ifdef|, |#ifndef|, |#elif|, or |#else| (all CPARSE:)
 *	<file> is the file where the directive <name> was encountered (a pathname)
 *	<start-line> is where the directive <name> was encountered
 *	<else> is T or NIL, saying whether a #else has been hit in this #if
 */

void
LIfdef (lstate, polarity)
	struct lexerstate *lstate;
	FLAG polarity;
{
	lispval name, defined;

	push(list(LCondP (lstate), polarity ? @'|#ifdef| : @'|ifndef|,
			LCurFile (lstate), LCurLine (lstate), @gl:nil),
		lstate->ifstack);
	if (LCondCheck (lstate)) return;	    /* outer cond failed: just return */
	name = LGetSym (lstate);
	LSkipToEOL (lstate);
	defined = lstate->cpstream (@:definedp, name);
	defined = polarity ? defined : !defined;
	rplaca(car(lstate->ifstack), defined ? @gl:t : @:failed);
}


void
LIf (lstate)
	struct lexerstate *lstate;
{
	lispval exptrue;

	push(list(LCondP (lstate), @'|#if|,
			LCurFile (lstate), LCurLine (lstate), @GL:NIL),
		lstate->ifstack);
	if (LCondCheck (lstate)) return;	   /* outer cond failed: just return */
	lstate->ifstate = IFSTART;		    /* set up for parsing expression */
	exptrue = lstate->cpstream (@:if);
	lstate->ifstate = IFNONE;
	LSkipToEOL (lstate);
	rplaca(car(lstate->ifstack), exptrue ? @GL:T : @:FAILED);
}


void
LElif (lstate)
	struct lexerstate *lstate;
{
	lispval exptrue, ifdesc;

	if (!lstate->ifstack) {
		LError (lstate->cpstream, 0, @"#elif without matching #if");
		return;
	}
	ifdesc = car(lstate->ifstack);
	if (cadr(cdddr(ifdesc))) {
		LError (lstate->cpstream, 0,
			   @"#elif: #else already appeared to go with ~A on line ~A",
			   cadr(ifdesc), cadddr(ifdesc));
		return;
	}
	rplaca(cdr(ifdesc), @'|#elif|);			/* update name of directive */
	rplaca(cdddr(ifdesc), LCurLine (lstate));	    /* update starting line */
	/* If previous cond succeeded, we're done until #endif */
	if (car(ifdesc) == @GL:T) rplaca(ifdesc, @GL:NIL);
	if (!car(ifdesc)) {
		LSkipToEOL (lstate);
		return;
	}
	lstate->ifstate = IFSTART;		    /* set up for parsing expression */
	exptrue = lstate->cpstream (@:if);
	lstate->ifstate = IFNONE;
	LSkipToEOL (lstate);
	rplaca(car(lstate->ifstack), exptrue ? @GL:T : @:FAILED);
}


void
LElse (lstate)
	struct lexerstate *lstate;
{
	lispval ifdesc;

	LSkipToEOL (lstate);
	if (!lstate->ifstack) {
		LError (lstate->cpstream, 0, @"#else without matching #if");
		return;
	}
	ifdesc = car(lstate->ifstack);
	if (cadr(cdddr(ifdesc))) {
		LError (lstate->cpstream, 0,
			   @"#else: #else already appeared to go with ~A on line ~A",
			   cadr(ifdesc), cadddr(ifdesc));
		return;
	}
	rplaca(cdr(ifdesc), @'|#else|);		  /* update kind of directive */
	rplaca(cdddr(ifdesc), LCurLine (lstate));	 /* update starting line */
	rplaca(cdr(cdddr(ifdesc)), @GL:T);
	if (car(ifdesc) == @:FAILED) rplaca(ifdesc, @GL:T);
	else if (car(ifdesc) == @GL:T) rplaca(ifdesc, @GL:NIL);
}


void
LEndif (lstate)
	struct lexerstate *lstate;
{
	LSkipToEOL (lstate);
	if (!lstate->ifstack) {
		LError (lstate->cpstream, 0, @"#endif without matching #if");
		return;
	}
	pop(lstate->ifstack);
}


void
LInclude (lstate)
	struct lexerstate *lstate;
{
	char c, *cptr, *token = lstate->tokbuf, *fnstart;
	lispval filename, where;

	if (LCondCheck (lstate)) return;
	LOADSTATE;
	SKIPGRAY;
	ENDCHECK;
	lstate->macexpstart = cptr - 1;					/* for LMacExpand */
	while (!isspace(c)  &&  c != '(') NEXT;
	SAVESTATE;
	*token = NUL;
	fnstart = lstate->tokbuf;		    /* fnstart: start of the filename */
	if (*fnstart == '"') {
		++fnstart;
		if (token[-1] != '"')
			LError (lstate->cpstream, -1, @"Incorrect #include syntax");
		else --token;
		where = @:current;
	}
	else if (*fnstart == '<') {
		++fnstart;
		if (token[-1] != '>')
			LError (lstate->cpstream, -1, @"Incorrect #include syntax");
		else --token;
		if (!strncmp (fnstart, "sys/", 4)) {
			fnstart += 4;
			where = @:include-sys;
		}
		else where = @:include;
	}
	else if (LSymbol (lstate, token, FALSE))
		where = @:current;							    /* VMS syntax */
	else {
		LMacExpand (lstate);				/* expand macro and recurse */
		LInclude (lstate);
		return;
	}
	LSkipToEOL (lstate);
	filename = LLispTok (fnstart, token);
	lstate->cpstream (@:include, filename, where);
}


void
LLine (lstate)
	struct lexerstate *lstate;
{
	LSkipToEOL (lstate);						 /* that's all for now */
}


int
LLisp (lstate)
	struct lexerstate *lstate;
{
	char c, *cptr, *token;
	lispval forms;

	LSkipToEOL (lstate);
	LOADSTATE;
	SKIP;
	for (;;) {
		if (c == '#') {
			SKIP;
			token = lstate->tokbuf;
			SKIPGRAY;
			while (c  &&  !isspace(c)) NEXT;
			*token = NUL;
			token = lstate->tokbuf;
			if (strcmp (token, "endlisp")) {
				SAVESTATE;
				LError (lstate->cpstream, 0,
					   @"No #directives allowed between #lisp and #endlisp");
				while (c  &&  c != '\n') SKIP;
			}
			else break;
		}
		while (c  &&  c != '\n') {
			SAVESTATE;
			push(lstate->cpstream (@:readlisp), forms);
			LOADSTATE;
			SKIPGRAY;
			if (c == ';') while (c  &&  c != '\n') SKIP;
		}
		if (c == 0) {
			SAVESTATE;
			LEOFErr (lstate);
		}
		SKIP;
	}
	SAVESTATE;
	lstate->yylval = cons(@'c:\#LISP, cons(@GL:NIL, nreverse(forms)));
	return LCondP (lstate) ? LISP_INCLUSION : 0;
}


lispval
LCondP (lstate)
	struct lexerstate *lstate;
{
	lispval ifstack = lstate->ifstack;

	return !ifstack  ||  @(gl:eq (gl:caar |ifstack|) gl:t);
}


/* Checks to see if we are reading in the scope of a failed conditional.
 * If so, skips to end of line and returns TRUE; else FALSE.  */
lispval
LCondCheck (lstate)
	struct lexerstate *lstate;
{
	char c, *cptr;

	LOADSTATE;
	if (!LCondP (lstate)) {
		while (c  &&  c != '\n') SKIP;
		SAVESTATE;
		return @GL:T;
		}
	SAVESTATE;
	return @GL:NIL;
}


/* Reads the next symbol token and returns an interned symbol.
 * Error if EOF or EOL encountered before any non-grayspace.  */
lispval
LGetSym (lstate)
	struct lexerstate *lstate;
{
	char c, *cptr, *token = lstate->tokbuf;

	LOADSTATE;
	SKIPGRAY;
	ENDCHECK;
	while (issym(c)) NEXT;	/* get symbol to be defined */
	SAVESTATE;
	return lstate->cpstream (@:intern, LLispTok (lstate->tokbuf, token));
}


void
LSkipToEOL (lstate)
	struct lexerstate *lstate;
{
	char c, *cptr;

	LOADSTATE;
	while (c  &&  c != '\n') SKIP;
	SAVESTATE;
}


/* Gathers the arguments to a preprocessor macro invocation;
 * stores them, as a Lisp list, in lstate->macargs. */
void
LMacArgs (lstate)
	struct lexerstate *lstate;
{
	int parendepth, GL$LENGTH();
	lispval arglist;
	char c, *cptr, *textbeg, *textend;
	char *tmacexpstart = lstate->macexpstart;		 /* save correct value */

	LOADSTATE;								    /* into c and cptr */
	if (c == ' ') {
		LWarn (lstate->cpstream, 0,
			  @"Warning: space before '(' in a macro invocation is nonportable");
		SKIP;
	}
	if (c != '(') {
		SAVESTATE;
		LError (lstate->cpstream, 0,
			   @"Missing argument(s) to preprocessor macro");
		return;
		}
	SKIP;											 /* skip '(' */
	SAVESTATE;
	lstate->lastbufptr = lstate->bufptr - 1;	    /* initiate text saving */
	lstate->savbufp = lstate->savbuf;
	lstate->macrostate = MARGGING;
	parendepth = 1;							   /* '(' already read */
	for (;;) switch (LToken (lstate)) {

	case RPAREN:
		if (--parendepth <= 0) {
			LSaveText (lstate);
			textend = lstate->savbufp;
			--textend;							  /* delete paren */
			while (textend > lstate->savbuf  &&  isspace(textend[-1]))
				--textend;
			*textend = NUL;				/* trim trailing whitespace */
			textbeg = lstate->savbuf;
			while (isspace(*textbeg)) ++textbeg;	    /* ... and leading */
			push(str2lisp (textbeg), arglist);
			lstate->savbufp = lstate->lastbufptr = NULL;	    /* saving off */
			lstate->macargs = nreverse(arglist);
			lstate->macexpstart = tmacexpstart;    /* put back saved value */
			return;
		}
		break;
	case COMMA:
		if (parendepth == 1) {
			LSaveText (lstate);
			textend = lstate->savbufp;
			--textend;							  /* delete comma */
			while (textend > lstate->savbuf  &&  isspace(textend[-1]))
				--textend;
			*textend = NUL;				/* trim trailing whitespace */
			textbeg = lstate->savbuf;
			while (isspace(*textbeg)) ++textbeg;	    /* ... and leading */
			push(str2lisp (textbeg), arglist);
			lstate->savbufp = lstate->savbuf;
		}
		break;
	case LPAREN:
		++parendepth;
		break;
	case ENDOFSTREAM:
		LError (lstate->cpstream, 0,
			   @"End of file in macro invocation: missing ')'?");
		lstate->savbufp = lstate->lastbufptr = NULL;		    /* saving off */
		lstate->macargs = @'GL:NIL;
		lstate->macexpstart = tmacexpstart;	    /* put back saved value */
		return;
	default:
		break;
	}
}


void
LMacExpand (lstate)
	struct lexerstate *lstate;
{
	FLAG arg_as_str, tok_cat;
	char *tokstart, *newbuf;
	char tcurchar, *tbufptr;
	lispval yylval = lstate->yylval;
	
	/* We set up to read from the string in yylval (which must have a
	 * trailing NUL, by the way). */
#lisp
	(gl:setq |newbuf.array| |yylval| |newbuf.index| 0)
	#+Chars (gl:setq |newbuf.array|
				  (gl:make-array (gl:array-length |newbuf.array|) 
							  :type gl:art-8b
							  :displaced-to |newbuf.array|))
#endlisp
	if (lstate->macparams == @'zeta-c:no-params)
		lstate->macparams = lstate->macargs = @GL:NIL;
	else {
		LMacArgs (lstate);
		if (GL$LENGTH (lstate->macargs)
		    != GL$LENGTH (lstate->macparams))
			LError (lstate->cpstream, 0,
				   @"Wrong number of arguments, ~D, to macro ~A",
				   GL$LENGTH (lstate->macargs), lstate->macro);
	}
	tcurchar = lstate->curchar;
	tbufptr = lstate->bufptr;
	lstate->curchar = *newbuf;			  /* set up to read from string */
	lstate->bufptr = newbuf + 1;
	lstate->lastbufptr = newbuf;				   /* initiate text saving */
	lstate->savbufp = lstate->savbuf;
	lstate->macrostate = MEXPANDING;
	arg_as_str = tok_cat = FALSE;
	for (;;) {
		/* store arg_as_str in lstate for LMacroSymbol (below) */
		lstate->macargstr = arg_as_str;
		lstate->mactokcat = tok_cat;
		tokstart = lstate->bufptr;
		switch (LToken (lstate)) {

		case MACTOKSTR:
			LSaveText (lstate);
			--lstate->savbufp;							/* delete # */
			arg_as_str = TRUE;
			continue;					   /* don't turn off arg_as_str */
/* error check: # followed by something besides a parameter */
		case MACTOKCAT:
			LSaveText (lstate);
			lstate->savbufp -= 2;	  				    /* delete ## */
			/* flush preceding whitespace */
			while (isspace(lstate->savbufp[-1])) --lstate->savbufp;
			tok_cat = TRUE;
			break;
		case ENDOFSTREAM:
			*lstate->savbufp = NUL;
			lstate->savbufp = lstate->lastbufptr = NULL;	   /* saving off */
			lstate->bufptr = tbufptr;		    /* restore input state */
			lstate->curchar = tcurchar;
			LScanExpansion (lstate);				    /* set up to scan */
			return;
		default:
			break;
		}
	}
}


void
LMacroSymbol (lstate)
	struct lexerstate *lstate;
{
	lispval arg, LMacArg();
	char *savbufp;
	int symlen, arglen, i;

	symlen = strlen (lstate->tokbuf);
	arg = LMacArg (lstate, symlen);
	if (arg) {
		arglen = (int)@(gl:array-active-length |arg|);
		LSaveText (lstate);
		savbufp = lstate->savbufp;
		savbufp -= symlen;
		if (lstate->macargstr) *savbufp++ = '"';
		for (i = 0; i < arglen; ++i)
			@(gl:aset (zeta-c:char-code (gl:aref |arg| |i|))
					|savbufp.array| (gl:+ |i| |savbufp.index|));
		savbufp += arglen;
		if (lstate->macargstr) *savbufp++ = '"';
		*savbufp = NUL;		/* ?? needed? */
		lstate->savbufp = savbufp;
	}
}


lispval
LMacArg (lstate, symlen)		/* if symbol is one of the parameters, returns */
	struct lexerstate *lstate;	  /* the corresponding argument, else NIL */
	int symlen;
{
	lispval params = lstate->macparams, args = lstate->macargs;
	char *tokbuf = lstate->tokbuf;
	lispval lisptok = LLispTok (tokbuf, tokbuf + symlen);

	if (params == @'zeta-c:no-params) return @GL:NIL;
	while (params) {
		if (@(zeta-c:array-compare |lisptok| (gl:car |params|)))
			return car(args);
		params = cdr(params);
		args = cdr(args);
	}
	return @GL:NIL;
}


/* Inserts macro expansion into current line text in place of call.
 * Expects expansion to be in lstate->savbuf, NUL-terminated.  */
void
LScanExpansion (lstate)
	struct lexerstate *lstate;
{
	int exp_len, line_len, new_len, call_len;
	/* "- 1" because next char has already been read; see last 2 lines below */
	char *bufptr = lstate->bufptr - 1, *call_start;

	/* First we make room */
	exp_len = strlen (lstate->savbuf);
	line_len = (int) @(gl:array-active-length |bufptr.array|);
	call_len = bufptr - lstate->macexpstart;
	call_start = bufptr - call_len;
	new_len = line_len + exp_len - call_len;
	if (new_len > 4096)
		LError (lstate->cpstream, 0,
			   @"More than 4K of line buffer being allocated, while expanding macro ~A;~%perhaps you have mutually recursive macros being expanded?",
			   lstate->macro);
/*	printf ("\nLScan: delta %d", exp_len - call_len); */
	if (new_len > (int)@(gl:array-length |bufptr.array|))
		@(gl:adjust-array-size |bufptr.array| |new_len|);
	@(gl:setf (gl:fill-pointer |bufptr.array|) |new_len|);
	memcpy (call_start + exp_len, bufptr, strlen (bufptr) + 1);
	/* Now copy the expansion into the gap */
	memcpy (call_start, lstate->savbuf, exp_len);
/*	@(gl:format gl:t "~&LScan: buffer \"~A\"" |bufptr.array|); */
	/* We're no longer expanding a macro */
	lstate->macrostate = MNONE;
	/* Finally, commence reading from the beginning of the expansion */
	lstate->curchar = *call_start;
	lstate->bufptr = call_start + 1;
}


void
LSaveText (lstate)
	struct lexerstate *lstate;
{
	int savlen;

	if (lstate->savbufp) {
		/* bufptr now points after the lookahead char; hence the "- 1"s */
		savlen = lstate->bufptr - lstate->lastbufptr - 1;
		memcpy (lstate->savbufp, lstate->lastbufptr, savlen);
		lstate->savbufp += savlen;
		lstate->lastbufptr = lstate->bufptr - 1;
	}
}


/* Despite its name, this doesn't necessarily work by lines (though it usually
 * does).  It reads the next piece of input, returning the first character of
 * it and setting *cpp to point to the rest of the characters.  Lines still
 * have the trailing newline on them, except that sequences of \<Newline> have
 * been deleted.  */
char
LNextLine (lstate, cpp)
	struct lexerstate *lstate;
	char **cpp;
{
	char *line;

	if (*cpp) lstate->bufptr = *cpp;
	LSaveText (lstate);
	if (lstate->macrostate == MEXPANDING)
		line = NULL;					  /* help out with macro expansion */
	else {
		lstate->cpstream (@:next-line);
		line = lstate->bufptr;
	}
	if (!line) {
		*cpp = "\0" + 1;
		if (lstate->savbufp) lstate->lastbufptr = *cpp - 1;
		return NUL;							   /* indicates real EOF */
	}
	*cpp = line + 1;
	if (lstate->savbufp) lstate->lastbufptr = line;
	return line[0];
}


/* Called by (:method c-parser :next-line). */
void
LSetBuf (lstate, buf)
	struct lexerstate *lstate;
	char *buf;
{
	lstate->bufptr = buf;
}


/* Returns the index in the buffer of the current character.
 * Called from Lisp for the purpose of printing error messages. */
int
LBufIndex (lstate)
	struct lexerstate *lstate;
{
	char *bufptr = lstate->bufptr;

	return (int)@|bufptr.index| - 1;
}


/* The following are copied from ZCCLIB.C, to reduce compilation dependencies. */

static char *
strcpy (s1, s2)
	char *s1, *s2;
{
	char *s1temp = s1;

	do *s1++ = *s2; while (*s2++);
	return s1temp;
}

static int
strcmp (s1, s2)
	char *s1, *s2;
{
	char c1, c2;

	while (*s1 || *s2) {
		c1 = *s1++;
		c2 = *s2++;
		if (c1 < c2) return -1;
		if (c1 > c2) return 1;
	}
	return 0;
}

static int
strncmp (s1, s2, n)
	char *s1, *s2;
	int n;
{
	char c1, c2;

	while (n-- > 0 && (*s1 || *s2)) {  /* Just like strcmp, but for  n chars */
		c1 = *s1++;
		c2 = *s2++;
		if (c1 < c2) return -1;
		if (c1 > c2) return 1;
	}
	return 0;
}

static int
strlen (s)
	char *s;
{
	char *s0 = s;

	while (*s) s++;
	return s - s0;
}

static lispval
str2lisp (s)
	char *s;
{
	return @(zeta-c:string-to-lisp |s.array| |s.index|);
}

#define ZETA_C_COMPARE_INCOMPARABLE_POINTERS
/* Note: arguments reversed WRT movmem. */
static char *
memcpy (dest, src, nbytes)
	char *dest, *src;
	int nbytes;
{
	char *tdest = dest;

	if (src > dest) while (nbytes-- > 0) *dest++ = *src++;
	else {
		src += nbytes;
		dest += nbytes;
		while (nbytes-- > 0) *--dest = *--src;
	}
	return tdest;
}


static int
toupper (c)
	char c;
{
	return (islower(c) ? _toupper(c) : c);
}


static int
tolower (c)
	char c;
{
	return (isupper(c) ? _tolower(c) : c);
}


/* End of ZCTOKEN.C */
