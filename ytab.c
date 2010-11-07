/* -*- Mode: C; Package: CParse; Tab-width: 5; Base: 10 -*- */
/* File: YTAB.C
 *
 *	This code has been placed in the public domain.
 *
 * This file contains the ZETA-C parser, as produced by Unix YACC.
 */


# line 9 "cyacc.y"
#define YYSTYPE	lispval
#define short		int		/* ZETA-C shorts are slow */

#define PREINC		1
#define POSTINC	2
#define FATAL		1

#define YYMAXDEPTH	512

lispval LMisc(), LWhere();
lispval PNil(), PCons(), PCar(), PCdr();
lispval PList();
lispval PReverse(), PAppend(), PStringCat();
lispval PIncrForm(), PDefinedp();
void PPushEnv(), PPopEnv(), PDeclare(), PError(), PAccept();

# define LOW 257
# define NUMBER 258
# define CHARCONST 259
# define STRING 260
# define STORAGECLASS 261
# define TYPE 262
# define TYPE_ADJECTIVE 263
# define TYPEDEF_TYPE 264
# define ELLIPSIS 265
# define STRUCT 266
# define ENUM 267
# define IF 268
# define ELSE 269
# define WHILE 270
# define DO 271
# define FOR 272
# define SWITCH 273
# define CASE 274
# define BREAK 275
# define CONTINUE 276
# define RETURN 277
# define GOTO 278
# define DEFAULT 279
# define SYMBOL 280
# define SALLOC 281
# define AALLOC 282
# define SEMI 283
# define COMMA 284
# define ASSIGN 285
# define OP_ASSIGN 286
# define QMARK 287
# define COLON 288
# define OR 289
# define AND 290
# define BIT_OR 291
# define BIT_XOR 292
# define AND_ADDRESS 293
# define EQUALITY 294
# define COMPARISON 295
# define SHIFT 296
# define PLUSMINUS 297
# define MUL_PTR 298
# define DIVMOD 299
# define UNARY 300
# define INCREMENT 301
# define NOT 302
# define BIT_NOT 303
# define SIZEOF 304
# define LBRACKET 305
# define RBRACKET 306
# define LPAREN 307
# define RPAREN 308
# define LBRACE 309
# define RBRACE 310
# define ELEMENT 311
# define HIGH 312
# define SHARPIF 313
# define DEFINED 314
# define LISP_INCLUSION 315
# define MACTOKSTR 316
# define MACTOKCAT 317
# define ENDOFSTREAM 318
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 593 "cyacc.y"


/* End of CYACC.Y */
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 8,
	0, 168,
	-2, 54,
-1, 25,
	283, 1,
	-2, 12,
-1, 29,
	261, 176,
	262, 176,
	263, 176,
	264, 176,
	266, 176,
	267, 176,
	309, 176,
	-2, 46,
-1, 32,
	283, 2,
	288, 2,
	306, 2,
	308, 2,
	313, 2,
	-2, 13,
-1, 67,
	283, 1,
	288, 1,
	306, 1,
	308, 1,
	313, 1,
	-2, 12,
-1, 148,
	283, 8,
	284, 8,
	288, 8,
	306, 8,
	308, 8,
	313, 8,
	-2, 13,
-1, 149,
	283, 10,
	284, 10,
	288, 10,
	306, 10,
	308, 10,
	313, 10,
	-2, 12,
-1, 174,
	283, 7,
	284, 7,
	288, 7,
	306, 7,
	308, 7,
	313, 7,
	-2, 13,
-1, 175,
	283, 9,
	284, 9,
	288, 9,
	306, 9,
	308, 9,
	313, 9,
	-2, 12,
-1, 181,
	308, 55,
	-2, 12,
-1, 279,
	307, 0,
	-2, 24,
	};
# define YYNPROD 186
# define YYLAST 1250
short yyact[]={

 116,  32, 105, 318, 159, 267,   6, 307,  32,  61,
  62, 113,  31,  64,  63, 158, 110,  32, 326, 155,
  32, 289,  47,  77,  32, 134, 339, 133, 132, 131,
 130, 129, 128, 127, 126, 125, 123, 124,  13, 122,
 276, 122, 168, 137, 325, 137,  66, 288, 188, 138,
  32, 138, 188, 262, 148, 108, 106, 291,  82, 189,
 261, 322,  87, 310, 114, 117, 118, 119, 120, 121,
  90, 139, 162, 106, 111, 287,  32,  32, 109, 275,
 320,  32,  32, 125, 123, 124,  89, 122, 142, 162,
 164, 137, 174, 176,  32,  32, 285, 138, 274, 163,
 185, 186, 279, 191, 194, 123, 124, 164, 122, 282,
 207, 281, 137, 147, 165, 166, 163, 189, 138, 169,
 170, 132, 131, 130, 129, 128, 127, 126, 125, 123,
 124, 203, 122, 209, 231, 282, 137, 281, 312,  91,
 254,  32, 138, 230,  92, 108, 210, 211, 212, 213,
 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
 224, 265,  92, 273, 253, 258, 191, 194, 109, 250,
   8, 257,  58,  59,  65,  37,  61,  62,  26, 142,
  64,  63,  14, 148,  15,  16,  17,  18,  19,  21,
  22,  23,  24,  20,  25,  50,  51,  27, 200, 252,
 199,  32, 200,  32, 199, 260,  90,  42, 249, 272,
 246,  43,  41, 110, 331,  46,  44,  45,  48, 245,
 228,  49,  89,  31, 268, 183,  90,   7,  52,  28,
 227, 200,   3, 199,  91, 109, 327, 251,  93, 169,
 247, 263,  89, 266, 197, 145,  32,  32,  32,  32,
 280, 158, 294, 295, 135, 136, 134,  92, 133, 132,
 131, 130, 129, 128, 127, 126, 125, 123, 124, 178,
 122, 155,  32, 201, 137, 306,  91, 328, 308, 144,
 138,  81,  80,  32,  76,  75, 296, 169, 108, 303,
 171, 297, 268, 200,  94, 199, 319, 290,  83,  92,
 309, 237, 280, 128, 127, 126, 125, 123, 124, 313,
 122, 109, 330, 315, 137, 244, 190,  72,  73, 301,
 138, 169,  32, 286,  32, 311, 111, 256, 333, 190,
 110, 329, 336, 319, 337, 127, 126, 125, 123, 124,
  32, 122, 323, 332, 248, 137, 340, 187, 173, 268,
 172, 138,  85,  79, 338,  58,  59,  65,  37,  61,
  62,  26, 169,  64,  63,  14, 334,  15,  16,  17,
  18,  19,  21,  22,  23,  24,  20,  25,  50,  51,
  27,  84, 126, 125, 123, 124,  74, 122, 324, 167,
  42, 137, 321, 152,  43,  41, 229, 138,  46,  44,
  45,  48,  90, 103,  49,  79,  31,  58,  59,  65,
 157,  52,  28,  78, 150, 151,  10,  14,  89,  15,
  16,  17,  18,  19,  21,  22,  23,  24,  20,  25,
  50,  51,  27, 255, 160, 154,  69,  12,  58,  59,
  65,  98,  42, 106,  33,  71,  43,  41,   9, 302,
  46,  44,  45,  48, 243,  30,  49,   5,  31, 104,
 115,  50,  51,  52,  28,  69,   4,  58,  59,  65,
   2,  61,  62, 113, 271,  64,  63, 101, 241,  11,
  34,  53,  44,  45,  48, 270, 100,  68,  29,  67,
  50,  51, 107,  69,  52,  58,  59,  65, 305, 198,
 317, 316,  42,  97, 292, 240,  43,  41, 236,  57,
  46,  44,  45,  48,  56, 161,  49, 115,  50,  51,
  55,  54,  36,  52,  69, 141,  58,  59,  65,  95,
  42, 180,  96,  60,  43,  41,  40, 177,  46,  44,
  45,  48, 202,  39,  49,   1, 269, 335, 115,  50,
  51,  52,   0, 102, 208,  69,   0,  58,  59,  65,
   0,  42,  35, 183,   0,  43,  41, 239,   0,  46,
  44,  45,  48, 184, 182,  49,   0, 269, 304, 181,
  50,  51,  52,   0,  69,   0,  58,  59,  65, 235,
 206, 314,  42,  99, 259,   0,  43,  41, 242,   0,
  46,  44,  45,  48, 192, 195,  49, 178, 115,  50,
  51,   0,   0,  52,  69,   0,  58,  59,  65, 143,
  38,  42, 255, 102, 102,  43,  41, 283, 284,  46,
  44,  45,  48, 193, 196,  49,   0, 269, 149,  50,
  51,   0,  52,  69,   0,  58,  59,  65,  61,  62,
 113,  42,  64,  63,   0,  43,  41, 112,   0,  46,
  44,  45,  48,  99,  99,  49, 226, 205,  50,  51,
 232, 233,  52,  69,   0,  58,  59,  65, 184, 264,
  42,   0,   0,   0,  43,  41,   0, 298,  46,  44,
  45,  48,   0,   0,  49, 204,   0,  67,  50,  51,
  86,  52,  69,   0,  58,  59,  65,   0,   0,   0,
  42,   0,   0,   0,  43,  41,   0,   0,  46,  44,
  45,  48,   0,   0,  49,   0, 115,  50,  51,   0,
  69,  52,  58,  59,  65,   0,   0,   0,   0,  42,
   0,   0,   0,  43,  41,   0,   0,  46,  44,  45,
  48,   0,   0,  49,  67,  50,  51,   0,   0,  69,
  52,  58,  59,  65,   0,   0,   0,  42,   0,   0,
   0,  43,  41,   0,   0,  46,  44,  45,  48,   0,
  38,  49,   0, 175,  50,  51,   0,  69,  52,  58,
  59,  65,   0,   0,   0,   0,  42,   0,   0,   0,
  43,  41,   0,   0,  46,  44,  45,  48,   0,   0,
  49, 149,  50,  51,   0,   0,  69,  52,  58,  59,
  65,   0,  38,   0,  42,   0,   0,   0,  43,  41,
   0,   0,  46,  44,  45,  48,   0,   0,  49,   0,
 115,  50,  51,   0,   0,  52,  37,  61,  62, 113,
   0,  64,  63,  42,   0,   0,   0,  43,  41,   0,
 293,  46,  44,  45,  48,   0,   0, 140,   0,   0,
   0, 135, 136, 134,  52, 133, 132, 131, 130, 129,
 128, 127, 126, 125, 123, 124,   0, 122,   0,   0,
   0, 137, 278,   0,  31,   0,   0, 138, 135, 136,
 134, 277, 133, 132, 131, 130, 129, 128, 127, 126,
 125, 123, 124,   0, 122,   0,   0,   0, 137,   0,
   0, 135, 136, 134, 138, 133, 132, 131, 130, 129,
 128, 127, 126, 125, 123, 124,   0, 122,   0,   0,
   0, 137,   0,   0,   0,   0,   0, 138, 131, 130,
 129, 128, 127, 126, 125, 123, 124,   0, 122, 179,
   0,   0, 137,   0,   0,   0,   0,   0, 138, 130,
 129, 128, 127, 126, 125, 123, 124,   0, 122,   0,
   0,   0, 137,   0,  88,   0,   0,   0, 138, 129,
 128, 127, 126, 125, 123, 124,   0, 122,   0,   0,
   0, 137,   0,   0,   0,   0,   0, 138,   0,   0,
   0,   0, 146,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0, 153, 156,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0, 225,   0,
   0,   0,   0,   0,   0,   0,   0, 234,   0,   0,
   0,   0,   0,   0,   0, 238,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0, 299,   0, 300,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0, 238 };
short yypact[]={

 -86,-1000,-1000,-1000,-1000,-1000,-1000, 474,-1000,-1000,
  34,-1000,-1000, 103, -22, -23, 149, -25, -26, 474,
  10,  98,  69, 417, 138, -50,   6,-1000,-1000,-1000,
 179,-1000,   4,  46,  42,-1000,-1000, 386,-1000,-1000,
-1000, 446, 446, 446, 446, 446, 446, 636, 560, 209,
 -28, -62,-194, 531,-1000, 152,-1000,-1000,-1000,-1000,
 133,-1000,-1000, -38, -58,-1000,-309,  -8,-1000,-1000,
-1000,-1000,-1000,-191,-1000, 474, 474, 119,   6,-1000,
 474, 474,   2,-1000,-1000,-1000,-1000,  67,  65,-1000,
-1000, 503, 299, 149, 149,  64,-259,-190,  45,-1000,
 179, 179,-1000,-1000, -63, -12,-1000,-1000, 446, 387,
 446,-1000,-1000,-1000,-260,-163, -72,-260,-260,-260,
-260,-260,-1000, 446, 446, 446, 446, 446, 446, 446,
 446, 446, 446, 446, 446, 446, 446, 446, 138,-260,
 209, -78, -88,-164, 386, 386,-1000, 138, -72,-163,
-1000,-1000,-1000,-290,-1000, 138,-294,-1000,-1000,-1000,
 585,  32,-1000,-191,-191, -89, -98, -67,  61,-1000,
-100,-1000,-1000,-1000, -72,-163,-139, -71,-1000,-109,
-144,  -8, 358,  43,  42,-1000,-1000,-1000,-137,-143,
-191,-103,-248,-255,-107,-190,-259, -39,-1000,-147,
 474, 328,  97, 636,-1000,-145,-210,-229,-268, 636,
-260,-260,-193,-214,  86,  40,   9, 696, 677, 657,
-169, 613, 636, 636, 586,-1000,-206, 446,-1000,-196,
-164,-164,-212,  39,-233,-1000,-263,-1000,  12,-1000,
-253,-1000,-1000,-191,-1000, 149, 149, 474, 474,-297,
-1000,-1000,-1000,-1000, 138,-1000, 138,-1000,-1000,-1000,
-1000,-1000,-1000,  35, 169,-1000, -17,-1000, 636, 268,
-303, 149,-1000,-1000,-1000,-1000,-1000, 446,-1000, 180,
-260,-245, 474,-170,-196,-1000, 446,-1000,-1000, 138,
 446,-1000,-1000,-208, 123,-1000,-247,  59,-1000,-1000,
-1000, 108,-1000,-1000,-1000,-266,-1000,-1000,-1000,-262,
-1000, -70,-1000, -31,-1000, 636,  48,  28,-1000, -74,
 446, 149,-1000, 474,-1000,-1000, 237,-1000,-1000,-1000,
-208, 446, 636,-1000,-282,-1000,-1000,-1000, 636, 149,
-1000 };
short yypgo[]={

   0, 545,  38,   0, 444, 480,  22, 481, 543, 536,
 525, 959, 487,  42, 533, 562, 522, 531, 478, 454,
 529, 619, 521, 520, 514, 509, 435, 508, 301, 410,
 505, 504, 501, 500,   3,   2, 441, 403, 499,   5,
 498, 396, 437, 492, 434, 485, 474,   6, 470, 466,
 457, 448, 445, 416, 532, 503 };
short yyr1[]={

   0,   2,   2,   2,   2,   4,   4,   4,   4,   5,
   5,   7,   6,   6,   3,   3,   3,   3,   3,   3,
   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,
   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,
   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,
   3,   3,   3,   3,   3,  11,  11,   9,  13,  13,
   8,   8,   8,  14,  14,  12,  12,  15,  16,  16,
  17,  17,  17,  17,  18,  19,  19,  19,  21,  21,
  21,  21,  21,  23,  23,  22,  22,  24,  24,  24,
  26,  27,  27,  28,  28,  25,  25,  25,  29,  30,
  30,  31,  32,  32,  33,  33,  34,  34,  34,  20,
  20,  36,  36,  37,  37,  35,  35,  35,  35,  35,
  35,  38,  39,  39,  39,  39,  40,  40,  10,  41,
  41,  41,  41,  41,  43,  42,  44,  44,  45,  45,
  46,  46,  47,  47,  47,  47,  47,  47,  47,  47,
  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,
  47,  47,   1,   1,  48,  48,  48,  48,  48,  52,
  49,  50,  50,  50,  53,  51,  51,  51,  55,  55,
  55,  55,  54,  54,  54,  54 };
short yyr2[]={

   0,   1,   1,   1,   1,   3,   3,   3,   2,   3,
   2,   2,   1,   1,   1,   1,   2,   2,   2,   2,
   2,   2,   2,   2,   4,   4,   4,   6,   3,   3,
   3,   3,   3,   3,   3,   3,   3,   3,   3,   5,
   3,   3,   4,   3,   2,   4,   1,   4,   4,   3,
   4,   4,   4,   4,   1,   1,   1,   3,   1,   0,
   1,   1,   1,   1,   2,   1,   1,   3,   4,   4,
   1,   2,   3,   3,   3,   2,   1,   1,   1,   2,
   1,   1,   1,   1,   2,   1,   1,   2,   2,   3,
   3,   1,   3,   1,   3,   2,   2,   3,   3,   2,
   0,   3,   1,   0,   1,   3,   1,   3,   2,   1,
   0,   1,   3,   1,   2,   1,   1,   3,   2,   3,
   4,   2,   1,   2,   3,   4,   1,   3,   2,   0,
   3,   2,   3,   4,   0,   5,   2,   0,   0,   1,
   2,   1,   1,   2,   5,   7,   5,   6,   9,   5,
   3,   2,   2,   2,   2,   3,   3,   3,   3,   1,
   1,   2,   1,   1,   1,   1,   1,   3,   1,   0,
   4,   2,   4,   1,   2,   1,   1,   2,   1,   3,
   2,   3,   1,   3,   2,   3 };
short yychk[]={

-1000,  -1, -48, 318, -49, -50, -47, 313, 256, -51,
 -53, -18, -42,  -2, 268, 270, 271, 272, 273, 274,
 279, 275, 276, 277, 278, 280, 264, 283, 315, -12,
 -19, 309,  -3,  -4,  -5, -15, -16, 261, -21,  -8,
  -9, 298, 293, 297, 302, 303, 301,  -6, 304, 307,
 281, 282, 314,  -7, -22, -23, -24, -25, 258, 259,
 -14, 262, 263, 267, 266, 260,  -2, 280, -12, 256,
 283, -52, 283, 284, 283, 307, 307, -47, 264, 256,
 307, 307,  -2, 288, 283, 283, 283,  -2, -11, 280,
 264, 284, 307, 288, 288, -20, -54, -55, -36, -15,
 307, 298, -16, -37, 280, -35, 264, -43, 284, 307,
 284, 284, -21, 264,  -6, 280,  -3,  -6,  -6,  -6,
  -6,  -6, 301, 298, 299, 297, 296, 295, 294, 293,
 292, 291, 290, 289, 287, 285, 286, 305, 311,  -6,
 307, -10,  -2, -21, 307, 307, -11, 307,  -3, 280,
 262, 263, 260, -11, -26, 309, -11, -29, 309, 313,
 -44, -36, 280, 307, 298,  -2,  -2, 270, -13,  -2,
  -2, 288, 283, 283,  -3, 280,  -3,  -4, 308, -11,
 -17, 280,  -7, 264,  -5, -47, -47, 283, 307, 307,
 284, -35, -55, -54, -35, -55, -54, 307, -38, 307,
 305, 285, -44,  -6, 308, 280,  -5,  -3,  -4,  -6,
  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,
  -6,  -6,  -6,  -6,  -6, -11, -10, 308, 308, -41,
 307, 298, -10, -10, -11, -26, -27, -28, -11, -29,
 -30, -18, -42, -19, 283, 308, 308, 307, 283, 308,
 308, 308, 308, 308, 284, 264, 284, 308, 308, -37,
 308, 308, 308, 280,  -7, 308, -13, -39,  -6, 309,
 -45, -46, -47, 308, 308, 308, 308, 288, 306, 308,
  -6, 307, 305, -41, -41, 308, 284, 308, 310, 284,
 285, 310, -31, -21, -47, -47,  -2, -13, -42, -11,
 -11, 284, 280, 306, 310, -40, -39, 310, -47,  -6,
 308, -13, 308,  -6, -28,  -6, -32, -33, -34, -35,
 288, 269, 308, 283, 280, 310, 284, 306, 308, 283,
 284, 288,  -6, -47, -13, 310, -39, -34,  -6, 308,
 -47 };
short yydef[]={

   0,  -2, 162, 163, 164, 165, 166,   0,  -2, 169,
 175, 173, 142,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,  -2,  86, 159, 160,  -2,
 110, 134,  -2,   3,   4,  65,  66,  77,  76,  14,
  15,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,  78,  80,  81,  82,  60,  61,
  62,  85,  83,   0,   0,  63,   0,  -2,  46,  54,
 161, 137, 171,   0, 143,   0,   0,   0,   0,  54,
  59,   0,   0, 151, 152, 153, 154,   0,   0,  55,
  56,   0,   0,   0,   0,   0, 174, 177, 109, 182,
   0,   0, 178, 111, 115, 113, 116, 137,   0,   0,
   0,  11,  75,  86,  16,  12,  13,  17,  18,  19,
  20,  21,  22,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,  23,
   0,   0,   0, 129,   0,   0,  44,   0,  -2,  -2,
  79,  84,  64,  87,  88,   0,  95,  96, 100, 167,
   0,   0, 115,   0,   0,   0,   0,   0,   0,  58,
   0, 150, 155, 156,  -2,  -2,  13,   0,  67,   0,
   0,  -2,   0,  56,  70, 157, 158,  74,   0,   0,
   0,   0,   0,   0, 118, 180, 184,   0, 114,   0,
  59,   0, 138,   5,  49,  12,   0,  13,   0,   6,
  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,
  38,   0,  40,  41,   0,  43,   0,   0,  57, 128,
 129, 129,   0,   0,   0,  89,   0,  91,  93,  97,
   0, 136, 170, 110, 172,   0,   0,   0,  59,   0,
  47,  48,  68,  69,   0,  71,   0, 185, 181, 112,
 117, 179, 183,  55,   0, 119,   0, 121, 122,   0,
   0, 139, 141,  50,  51,  52,  53,   0,  42,  -2,
  25,   0,  59,   0, 131,  26,   0,  45,  90,   0,
   0,  98,  99, 103, 144, 146,   0,   0, 149,  73,
  72,   0,  10, 120, 123,   0, 126, 135, 140,  39,
 132,   0, 130,   0,  92,  94,   0, 102, 104, 106,
   0,   0, 147,  59,   9, 124,   0, 133,  27, 101,
   0,   0, 108, 145,   0, 125, 127, 105, 107,   0,
 148 };
#
# define YYFLAG -1000
# define YYERROR goto yyerrlab
# define YYACCEPT return(0)
# define YYABORT return(1)

/*	parser for yacc output	*/

#ifdef YYDEBUG
int yydebug = 0; /* 1 for debugging */
#endif

struct yyprsr_st {
	int yychar;		/* current input token number */
	int yynerrs;		/* number of errors */
	int yyerrflag;		/* error recovery flag */
	YYSTYPE yylval;	/* lexer token */
	lispval cpstream;	/* pointer back to C-PARSER instance that made me */
	YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
	};

yyreset(yyprsr)
	struct yyprsr_st *yyprsr;
{
	yyprsr->yychar = -1; /* current input token number */
	}

struct yyprsr_st *
yynew()
{
	return (struct yyprsr_st *) calloc (1, sizeof(struct yyprsr_st));
	}

yyinit(yyprsr, cpstream)
	struct yyprsr_st *yyprsr;
	lispval cpstream;
{
	yyprsr->cpstream = cpstream;
	}

yysetlval(lval, yyprsr)
	YYSTYPE lval;
	struct yyprsr_st *yyprsr;
{
	yyprsr->yylval = lval;
	}

yyparse(yyprsr)
	struct yyprsr_st *yyprsr;
{
	int yys[YYMAXDEPTH];
	int yyj, yym;
	register YYSTYPE *yypvt;
	register int yystate, *yyps, yyn;
	register YYSTYPE *yypv;
	register int *yyxi;
	int yychar = yyprsr->yychar;
	int yynerrs;
	YYSTYPE yyval;

#define SELF yyprsr->cpstream

	yystate = 0;
	yychar = -1;
	yynerrs = 0;
	yyerrflag = 0;
	yyps= &yys[-1];
	yypv= &yyprsr->yyv[-1];

 yystack:    /* put a state and value onto the stack */

#ifdef YYDEBUG
	if( yydebug  ) printf( "state %d, char 0%o\n", yystate, yychar );
#endif
		if( ++yyps> &yys[YYMAXDEPTH] ) {
			yyerror(SELF, "yacc stack overflow");
			yyprsr->yychar = yychar;
			return(1);
			}
		*yyps = yystate;
		++yypv;
		*yypv = yyval;

 yynewstate:

	yyn = yypact[yystate];

	if( yyn<= YYFLAG ) goto yydefault; /* simple state */

	if( yychar<0 ) if( (yychar=yylex(SELF))<0 ) yychar=0;
	if( (yyn += yychar)<0 || yyn >= YYLAST ) goto yydefault;

	if( yychk[ yyn=yyact[ yyn ] ] == yychar ){ /* valid shift */
		yychar = -1;
		yyval = yyprsr->yylval;
		yystate = yyn;
		if( yyerrflag > 0 ) --yyerrflag;
		goto yystack;
		}

 yydefault:
	/* default state action */

	if( (yyn=yydef[yystate]) == -2 ) {
		if( yychar<0 ) if( (yychar=yylex(SELF))<0 ) yychar = 0;
		/* look through exception table */

		for( yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate) ; yyxi += 2 ) ; /* VOID */

		while( *(yyxi+=2) >= 0 ){
			if( *yyxi == yychar ) break;
			}
		if( (yyn = yyxi[1]) < 0 ) {
			yyprsr->yychar = yychar;
			return(0);   /* accept */
			}
		}

	if( yyn == 0 ){ /* error */
		/* error ... attempt to resume parsing */

		switch( yyerrflag ){

		case 0:   /* brand new error */

			yyerror(SELF, "syntax error");
		yyerrlab:
			++yynerrs;

		case 1:
		case 2: /* incompletely recovered error ... try again */

			yyerrflag = 3;

			/* find a state where "error" is a legal shift action */

			while ( yyps >= yys ) {
			   yyn = yypact[*yyps] + YYERRCODE;
			   if( yyn>= 0 && yyn < YYLAST && yychk[yyact[yyn]] == YYERRCODE ){
			      yystate = yyact[yyn];  /* simulate a shift of "error" */
			      goto yystack;
			      }
			   yyn = yypact[*yyps];

			   /* the current yyps has no shift on "error", pop stack */

#ifdef YYDEBUG
			   if( yydebug ) printf( "error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1] );
#endif
			   --yyps;
			   --yypv;
			   }

			/* there is no state on the stack with an error shift ... abort */

	yyabort:
			yyprsr->yychar = yychar;
			return(1);


		case 3:  /* no shift yet; clobber input char */

#ifdef YYDEBUG
			if( yydebug ) printf( "error recovery discards char %d\n", yychar );
#endif

			if( yychar == ENDOFSTREAM ) goto yyabort; /* don't discard EOF, quit */
			yychar = -1;
			goto yynewstate;   /* try again in the same state */

			}

		}

	/* reduction by production yyn */

#ifdef YYDEBUG
		if( yydebug ) printf("reduce %d\n",yyn);
#endif
		yyps -= yyr2[yyn];
		yypvt = yypv;
		yypv -= yyr2[yyn];
		yyval = yypv[1];
		yym=yyn;
			/* consult goto table to find next state */
		yyn = yyr1[yyn];
		yyj = yypgo[yyn] + *yyps + 1;
		if( yyj>=YYLAST || yychk[ yystate = yyact[yyj] ] != -yyn ) yystate = yyact[yypgo[yyn]];
		yyprsr->yychar = yychar;			/* In case we're thrown out of */
		yyprsr->yynerrs = yynerrs;		/* In case someone wants to look */
		yyprsr->yyerrflag = yyerrflag;	/* likewise */
		switch(yym){
			
		case 3:
# line 98 "cyacc.y"
			{yyval = PCons (LMisc ("progn+"), PReverse (yypvt[-0]));} break;
		case 4:
# line 100 "cyacc.y"
			{yyval = PCons (LMisc ("progn+"), PReverse (yypvt[-0]));} break;
		case 5:
# line 105 "cyacc.y"
			{yyval = PList (yypvt[-0], yypvt[-2]);} break;
		case 6:
# line 107 "cyacc.y"
			{yyval = PCons (yypvt[-0], yypvt[-2]);} break;
		case 7:
# line 109 "cyacc.y"
			{yyval = PList (yypvt[-0], yypvt[-2]);} break;
		case 8:
# line 111 "cyacc.y"
			{yyval = PCons (yypvt[-0], yypvt[-1]);} break;
		case 9:
# line 115 "cyacc.y"
			{yyval = PList (yypvt[-0], yypvt[-2]);} break;
		case 10:
# line 117 "cyacc.y"
			{yyval = PCons (yypvt[-0], yypvt[-1]);} break;
		case 11:
# line 123 "cyacc.y"
			{yyval = yypvt[-1];} break;
		case 16:
# line 139 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-0]);} break;
		case 17:
# line 141 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-0]);} break;
		case 18:
# line 143 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-0]);} break;
		case 19:
# line 145 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-0]);} break;
		case 20:
# line 147 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-0]);} break;
		case 21:
# line 149 "cyacc.y"
			{yyval = PIncrForm (yypvt[-1], yypvt[-0], PREINC);} break;
		case 22:
# line 151 "cyacc.y"
			{yyval = PIncrForm (yypvt[-0], yypvt[-1], POSTINC);} break;
		case 23:
# line 153 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-0]);} break;
		case 24:
# line 155 "cyacc.y"
			{yyval = PList (yypvt[-3], yypvt[-1], PNil());} break;
		case 25:
# line 157 "cyacc.y"
			{yyval = PList (LMisc ("cast+"), yypvt[-2], yypvt[-0]);} break;
		case 26:
# line 159 "cyacc.y"
			{yyval = PList (yypvt[-3], yypvt[-1]);} break;
		case 27:
# line 161 "cyacc.y"
			{yyval = PList (yypvt[-5], yypvt[-3], yypvt[-1]);} break;
		case 28:
# line 163 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 29:
# line 165 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 30:
# line 167 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 31:
# line 169 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 32:
# line 171 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 33:
# line 173 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 34:
# line 175 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 35:
# line 177 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 36:
# line 179 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 37:
# line 181 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 38:
# line 183 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 39:
# line 185 "cyacc.y"
			{yyval = PList (LMisc ("?:"), yypvt[-4], yypvt[-2], yypvt[-0]);} break;
		case 40:
# line 187 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 41:
# line 189 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 42:
# line 191 "cyacc.y"
			{yyval = PList (LMisc ("[]"), yypvt[-3], yypvt[-1]);} break;
		case 43:
# line 193 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 44:
# line 195 "cyacc.y"
			{yyval = PDefinedp (yyprsr->cpstream, yypvt[-0]);} break;
		case 45:
# line 197 "cyacc.y"
			{yyval = PDefinedp (yyprsr->cpstream, yypvt[-1]);} break;
		case 47:
# line 203 "cyacc.y"
			{yyval = PList (yypvt[-3], yypvt[-1]);} break;
		case 48:
# line 205 "cyacc.y"
			{yyval = PCons (yypvt[-3], PReverse (yypvt[-1]));} break;
		case 49:
# line 207 "cyacc.y"
			{yyval = PList (yypvt[-2]);} break;
		case 50:
# line 209 "cyacc.y"
			{yyval = PList (yypvt[-3], yypvt[-1]);} break;
		case 51:
# line 211 "cyacc.y"
			{yyval = PCons (yypvt[-3], PReverse (yypvt[-1]));} break;
		case 52:
# line 213 "cyacc.y"
			{yyval = PList (yypvt[-3], yypvt[-1]);} break;
		case 53:
# line 215 "cyacc.y"
			{yyval = PCons (yypvt[-3], PReverse (yypvt[-1]));} break;
		case 54:
# line 217 "cyacc.y"
			{PError (SELF, "Expression syntax"); yyval = yypvt[-0];} break;
		case 57:
# line 229 "cyacc.y"
			{yyval = yypvt[-1];} break;
		case 59:
# line 235 "cyacc.y"
			{yyval = PNil();} break;
		case 64:
# line 249 "cyacc.y"
			{yyval = PStringCat (yypvt[-1], yypvt[-0]);} break;
		case 67:
# line 261 "cyacc.y"
			{yyval = PList (yypvt[-2]);} break;
		case 68:
# line 265 "cyacc.y"
			{yyval = PList (yypvt[-3], yypvt[-1]);} break;
		case 69:
# line 267 "cyacc.y"
			{yyval = PCons (yypvt[-3], PReverse (yypvt[-1]));} break;
		case 71:
# line 273 "cyacc.y"
			{yyval = PCons (yypvt[-0], yypvt[-1]);} break;
		case 72:
# line 275 "cyacc.y"
			{yyval = PList (yypvt[-0], yypvt[-2]);} break;
		case 73:
# line 277 "cyacc.y"
			{yyval = PCons (yypvt[-0], yypvt[-2]);} break;
		case 74:
# line 283 "cyacc.y"
			{yyval = PCons (yypvt[-2], yypvt[-1]);} break;
		case 75:
# line 288 "cyacc.y"
			{yyval = PCons (yypvt[-1], yypvt[-0]);} break;
		case 77:
# line 294 "cyacc.y"
			{yyval = PList (yypvt[-0]);} break;
		case 78:
# line 299 "cyacc.y"
			{yyval = PList (yypvt[-0]);} break;
		case 79:
# line 301 "cyacc.y"
			{yyval = PAppend (PReverse (yypvt[-1]), PList (yypvt[-0]));} break;
		case 80:
# line 303 "cyacc.y"
			{yyval = PReverse (yypvt[-0]);} break;
		case 81:
# line 305 "cyacc.y"
			{yyval = PList (yypvt[-0]);} break;
		case 82:
# line 307 "cyacc.y"
			{yyval = PList (yypvt[-0]);} break;
		case 83:
# line 311 "cyacc.y"
			{yyval = PList (yypvt[-0]);} break;
		case 84:
# line 313 "cyacc.y"
			{yyval = PCons (yypvt[-0], yypvt[-1]);} break;
		case 87:
# line 323 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-0]);} break;
		case 88:
# line 325 "cyacc.y"
			{yyval = PCons (yypvt[-1], PCons (PNil(), yypvt[-0]));} break;
		case 89:
# line 327 "cyacc.y"
			{yyval = PCons (yypvt[-2], PCons (yypvt[-1], yypvt[-0]));} break;
		case 90:
# line 331 "cyacc.y"
			{yyval = PReverse (yypvt[-1]);} break;
		case 91:
# line 335 "cyacc.y"
			{yyval = PList (yypvt[-0]);} break;
		case 92:
# line 337 "cyacc.y"
			{yyval = PCons (yypvt[-0], yypvt[-2]);} break;
		case 94:
# line 343 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		case 95:
# line 347 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-0]);} break;
		case 96:
# line 349 "cyacc.y"
			{yyval = PCons (yypvt[-1], PCons (PNil(), yypvt[-0]));} break;
		case 97:
# line 351 "cyacc.y"
			{yyval = PCons (yypvt[-2], PCons (yypvt[-1], yypvt[-0]));} break;
		case 98:
# line 355 "cyacc.y"
			{yyval = PReverse (yypvt[-1]);} break;
		case 99:
# line 359 "cyacc.y"
			{yyval = PCons (yypvt[-0], yypvt[-1]);} break;
		case 100:
# line 361 "cyacc.y"
			{yyval = PNil();} break;
		case 101:
# line 365 "cyacc.y"
			{yyval = PCons (yypvt[-2], yypvt[-1]);} break;
		case 102:
# line 369 "cyacc.y"
			{yyval = PReverse (yypvt[-0]);} break;
		case 103:
# line 371 "cyacc.y"
			{yyval = PNil();} break;
		case 104:
# line 375 "cyacc.y"
			{yyval = PList (yypvt[-0]);} break;
		case 105:
# line 377 "cyacc.y"
			{yyval = PCons (yypvt[-0], yypvt[-2]);} break;
		case 107:
# line 383 "cyacc.y"
			{yyval = PList (LMisc ("bits+"), yypvt[-2], yypvt[-0]);} break;
		case 108:
# line 385 "cyacc.y"
			{yyval = PList (LMisc ("bits+"), PNil(), yypvt[-0]);} break;
		case 109:
# line 389 "cyacc.y"
			{yyval = PReverse (yypvt[-0]);} break;
		case 110:
# line 391 "cyacc.y"
			{yyval = PNil();} break;
		case 111:
# line 395 "cyacc.y"
			{yyval = PList (yypvt[-0]);} break;
		case 112:
# line 397 "cyacc.y"
			{yyval = PCons (yypvt[-0], yypvt[-2]);} break;
		case 114:
# line 403 "cyacc.y"
			{yyval = PList (PCar (yypvt[-0]), yypvt[-1], PCdr (yypvt[-0]));} break;
		case 117:
# line 411 "cyacc.y"
			{yyval = yypvt[-1];} break;
		case 118:
# line 413 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-0]);} break;
		case 119:
# line 415 "cyacc.y"
			{yyval = PList (LMisc ("fcn+"), yypvt[-2]);} break;
		case 120:
# line 417 "cyacc.y"
			{yyval = PList (LMisc ("[]"), yypvt[-3], yypvt[-1]);} break;
		case 121:
# line 421 "cyacc.y"
			{yyval = PCons (yypvt[-1], yypvt[-0]);} break;
		case 123:
# line 427 "cyacc.y"
			{yyval = PNil();} break;
		case 124:
# line 429 "cyacc.y"
			{yyval = PCons (LMisc ("list+"), PReverse (yypvt[-1]));} break;
		case 125:
# line 431 "cyacc.y"
			{yyval = PCons (LMisc ("list+"), PReverse (yypvt[-2]));} break;
		case 126:
# line 435 "cyacc.y"
			{yyval = PList (yypvt[-0]);} break;
		case 127:
# line 437 "cyacc.y"
			{yyval = PCons (yypvt[-0], yypvt[-2]);} break;
		case 128:
# line 441 "cyacc.y"
			{yyval = PCons (yypvt[-1], yypvt[-0]);} break;
		case 129:
# line 445 "cyacc.y"
			{yyval = PNil();} break;
		case 130:
# line 447 "cyacc.y"
			{yyval = yypvt[-1];} break;
		case 131:
# line 449 "cyacc.y"
			{yyval = PList (PCons (yypvt[-1], yypvt[-0]));} break;
		case 132:
# line 451 "cyacc.y"
			{yyval = PList (PCons (LMisc ("fcn+"), yypvt[-2]));} break;
		case 133:
# line 453 "cyacc.y"
			{yyval = PList (PList (LMisc ("[]"), PCar (yypvt[-3]), yypvt[-1]));} break;
		case 134:
# line 456 "cyacc.y"
			{PPushEnv(yyprsr->cpstream);} break;
		case 135:
# line 457 "cyacc.y"
			{yyval = PCons (LMisc ("block+"), PCons (PReverse (yypvt[-2]), yypvt[-1]));
				 PPopEnv(SELF);} break;
		case 136:
# line 462 "cyacc.y"
			{PDeclare (yyprsr->cpstream, yypvt[-0]); yyval = PCons (yypvt[-0], yypvt[-1]);} break;
		case 137:
# line 464 "cyacc.y"
			{yyval = PNil();} break;
		case 138:
# line 471 "cyacc.y"
			{yyval = PNil();} break;
		case 139:
# line 473 "cyacc.y"
			{yyval = PReverse(yypvt[-0]);} break;
		case 140:
# line 477 "cyacc.y"
			{yyval = PCons (yypvt[-0], yypvt[-1]);} break;
		case 141:
# line 479 "cyacc.y"
			{yyval = PList (yypvt[-0]);} break;
		case 144:
# line 487 "cyacc.y"
			{yyval = PList (yypvt[-4], yypvt[-2], yypvt[-0]);} break;
		case 145:
# line 489 "cyacc.y"
			{yyval = PList (yypvt[-6], yypvt[-4], yypvt[-2], yypvt[-0]);} break;
		case 146:
# line 491 "cyacc.y"
			{yyval = PList (yypvt[-4], yypvt[-2], yypvt[-0]);} break;
		case 147:
# line 493 "cyacc.y"
			{yyval = PList (yypvt[-5], yypvt[-4], yypvt[-1]);} break;
		case 148:
# line 495 "cyacc.y"
			{yyval = PList (yypvt[-8], yypvt[-6], yypvt[-4], yypvt[-2], yypvt[-0]);} break;
		case 149:
# line 497 "cyacc.y"
			{yyval = PList (yypvt[-4], yypvt[-2], yypvt[-0]);} break;
		case 150:
# line 499 "cyacc.y"
			{yyval = PList (yypvt[-2], yypvt[-1]);} break;
		case 152:
# line 503 "cyacc.y"
			{yyval = PList (yypvt[-1]);} break;
		case 153:
# line 505 "cyacc.y"
			{yyval = PList (yypvt[-1]);} break;
		case 154:
# line 507 "cyacc.y"
			{yyval = PList (yypvt[-1]);} break;
		case 155:
# line 509 "cyacc.y"
			{yyval = PList (yypvt[-2], yypvt[-1]);} break;
		case 156:
# line 511 "cyacc.y"
			{yyval = PList (yypvt[-2], yypvt[-1]);} break;
		case 157:
# line 514 "cyacc.y"
			{yyval = PList (LMisc ("label+"), yypvt[-2], yypvt[-0]);} break;
		case 158:
# line 516 "cyacc.y"
			{yyval = PList (LMisc ("label+"), yypvt[-2], yypvt[-0]);} break;
		case 159:
# line 518 "cyacc.y"
			{yyval = PNil();} break;
		case 161:
# line 522 "cyacc.y"
			{PError (SELF, "Statement syntax"); yyval = yypvt[-1];} break;
		case 162:
# line 526 "cyacc.y"
			{PAccept (yypvt[-0]);} break;
		case 163:
# line 528 "cyacc.y"
			{PAccept (PNil());} break;
		case 165:
# line 534 "cyacc.y"
			{PDeclare (yyprsr->cpstream, yypvt[-0]);			/* for C-SOURCE macro */
				 yyval = PCons (LMisc ("decl+"), PCons (LWhere(SELF), yypvt[-0]));} break;
		case 167:
# line 539 "cyacc.y"
			{yyval = yypvt[-1];} break;
		case 168:
# line 541 "cyacc.y"
			{PError (SELF, "External definition syntax", FATAL); yyval = yypvt[-0];} break;
		case 169:
# line 544 "cyacc.y"
			{PPushEnv(yyprsr->cpstream);} break;
		case 170:
# line 545 "cyacc.y"
			{yyval = PList (LMisc ("defunc+"), LWhere(SELF), yypvt[-3], PReverse (yypvt[-1]), yypvt[-0]);
				 PPopEnv(SELF);} break;
		case 172:
# line 552 "cyacc.y"
			{yyval = PAppend (yypvt[-3], PReverse (yypvt[-1]));} break;
		case 174:
# line 558 "cyacc.y"
			{yyval = PCons (yypvt[-1], PList (yypvt[-0]));} break;
		case 176:
# line 564 "cyacc.y"
			{yyval = PList (PNil(), PCons (LMisc ("fcn+"), yypvt[-0]));} break;
		case 177:
# line 566 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-0]);} break;
		case 178:
# line 570 "cyacc.y"
			{yyval = PCons (LMisc ("fcn+"), yypvt[-0]);} break;
		case 179:
# line 572 "cyacc.y"
			{yyval = yypvt[-1];} break;
		case 180:
# line 574 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-0]);} break;
		case 181:
# line 578 "cyacc.y"
			{yyval = PList (LMisc ("fcn+"), yypvt[-2]);} break;
		case 182:
# line 582 "cyacc.y"
			{yyval = PCons (LMisc ("fcn+"), yypvt[-0]);} break;
		case 183:
# line 584 "cyacc.y"
			{yyval = yypvt[-1];} break;
		case 184:
# line 586 "cyacc.y"
			{yyval = PList (yypvt[-1], yypvt[-0]);} break;
		case 185:
# line 590 "cyacc.y"
			{yyval = PList (LMisc ("fcn+"), yypvt[-2]);} break;
		}
		goto yystack;  /* stack new state and value */

	}
