/*
 CTYPE.H for ZETA-C: Macros for char manipulation.
 Note this file does not have a package specification, as it will be
 read into (potentially) many different packages.  */

#define	_U	01		/* Upper case alphabetic */
#define	_L	02		/* Lower case alphabetic */
#define	_N	04		/* Numeric (i.e. decimal digits) */
#define	_S	010		/* whiteSpace */
#define _P	020		/* Punctuation (i.e. misc printing chars) */
#define _C	040		/* Control characters */
#define	_X	0100		/* Hex digits A-F, a-f */

extern char c$_ctype_[];	/* Actual table in ZETA-C: SOURCE; ZCCLIB.C */

#define isalnum(c)	((c$_ctype_+1)[c]&(_U|_L|_N))
#define	isalpha(c)	((c$_ctype_+1)[c]&(_U|_L))
#define iscntrl(c)	((c$_ctype_+1)[c]&_C)
#define	isdigit(c)	((c$_ctype_+1)[c]&_N)
#define	isgraph(c)	((c$_ctype_+1)[c]&(_P|_U|_L|_N))
#define	islower(c)	((c$_ctype_+1)[c]&_L)
#define isprint(c)	((c$_ctype_+1)[c]&(_P|_U|_L|_N|_S))
#define ispunct(c)	((c$_ctype_+1)[c]&_P)
#define	isspace(c)	((c$_ctype_+1)[c]&_S)
#define	isupper(c)	((c$_ctype_+1)[c]&_U)
#define	isxdigit(c)	((c$_ctype_+1)[c]&(_N|_X))

/* N.b. the Lispm does not use traditional ASCII, so if you use
 * isascii(c), check the char codes.
 * E.g., (int)'\n' == 141 == 0215, so isascii('\n') == 0.  */
#define isascii(c)	((unsigned)(c)<=0177)

#define _toupper(c)	((c)-'a'+'A')
#define _tolower(c)	((c)-'A'+'a')
#define toascii(c)	((c)&0177)

/* End of CTYPE.H */
