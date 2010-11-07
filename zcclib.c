/*
 * This file contains the portion of the ZETA-C runtime library that's in C.
 */

#include "stdio.h"

#define FALSE		0
#define TRUE		1
#define NUL		'\0'

/* The symbols from package C: which are used in this code have already been
   interned and exported by ZCINIT.LISP. */

/* ================================================================ */
/* Routines in this file, in order (indented so as not to fire sectionizer). */

/* String manipulation. */
 char *c$strcpy(), *c$strncpy(), *c$strcat(), *c$strncat(), *c$strchr();
 int c$strcmp(), c$strncmp(), c$strlen(), c$strcspn();
 void *c$memcpy(), *c$memset(), *c$memchr();
 int c$memcmp();

/* Memory allocation. */
 void *c$malloc(), *c$calloc(), *c$realloc();

/* Handy stuff. */
 int c$toupper(), c$tolower();

/* Standard I/O library. */
 int c$scanf(), c$sscanf(), c$fscanf();

/* Emulated Unix System Calls. */
 void times();

char *
c$strcpy (s1, s2)
	char *s1, *s2;
{
	char *s1temp = s1;

	do *s1++ = *s2; while (*s2++);
	return s1temp;
}

char *
c$strncpy (s1, s2, n)
	char *s1, *s2;
	int n;
{
	char *s1temp = s1;

	while (--n >= 0) *s1++ = *s2 ? *s2++ : NUL;
	return s1temp;
}

char *
c$strcat (s1, s2)
	char *s1, *s2;
{
	strcpy (s1 + strlen (s1), s2);
	return s1;
}

char *
c$strncat (s1, s2, n)
	char *s1, *s2;
	int n;
{
	char *s1tmp;

	s1tmp = s1 + strlen (s1);	/* Remember where the original string ended */
	strncpy (s1tmp, s2, n);				    /* The real work happens here */
	s1tmp[n] = NUL;			  /* Must guarantee that result ends in NUL */
	return s1;
}


char *
c$strchr (s, c)
	char *s, c;
{
	do if (*s == c) return s; while (*s++);
	return NULL;
}


int
c$strcmp (s1, s2)
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

int
c$strncmp (s1, s2, n)
	char *s1, *s2;
	int n;
{
	char c1, c2;

	while (n-- > 0 && (*s1 || *s2)) {	 /* Just like strcmp, but for < n chars */
		c1 = *s1++;
		c2 = *s2++;
		if (c1 < c2) return -1;
		if (c1 > c2) return 1;
	}
	return 0;
}

int
c$strlen (s)
	char *s;
{
	char *s0 = s;

	while (*s) s++;
	return s - s0;
}

int
c$strcspn (s1, s2)
	char *s1, *s2;
{
	int n = 0;

	while (s1[n] &&					 /* WHILE not at the end of S1, and */
	       !strchr(s2, s1[n]))			 /* not not at one of S2s chars */
	     n++;
	return n;
}

/* Accessible from C as ZCLIB$str2lisp(). */
lispval
str2lisp (s)
	char *s;
{
	return @(zeta-c:string-to-lisp |s.array| |s.index|);
}

/* Note: arguments reversed WRT movmem.
 * The difference between memmove and memcpy is that memcpy doesn't handle
 * overlapping moves correctly. */
void *
c$memcpy (destv, srcv, nbytes)
	void *destv, *srcv;
	int nbytes;
{
	void *tdest = destv;
	char *dest = (char *)destv, *src = (char *)srcv;  /* Force byte pointers */

	while (nbytes-- > 0) *dest++ = *src++;
	return tdest;
}

#define ZETA_C_COMPARE_INCOMPARABLE_POINTERS
/* Note: arguments reversed WRT movmem. */
void *
c$memmove (destv, srcv, nbytes)
	void *destv, *srcv;
	int nbytes;
{
	void *tdest = destv;
	char *dest = (char *)destv, *src = (char *)srcv;  /* Force byte pointers */

	if (src > dest) while (nbytes-- > 0) *dest++ = *src++;
	else {
		src += nbytes;
		dest += nbytes;
		while (nbytes-- > 0) *--dest = *--src;
	}
	return tdest;
}
#undef ZETA_C_COMPARE_INCOMPARABLE_POINTERS

void *
c$memset (destv, c, nbytes)
	void *destv;
	char c;
	int nbytes;
{
	char *dest = (char *)destv;					 /* Force byte pointer */
	
	while (nbytes-- > 0) *dest++ = c;
	return destv;
}

int
c$memcmp (m1v, m2v, nbytes)			/* Just like strncmp, but ignores NUL */
	void *m1v, *m2v;
	int nbytes;
{
	char *m1 = (char *)m1v, *m2 = (char *)m2v;		/* Force byte pointers */
	char c1, c2;

	while (nbytes-- > 0) {
		c1 = *m1++;
		c2 = *m2++;
		if (c1 < c2) return -1;
		if (c1 > c2) return 1;
	}
	return 0;
}

void *
c$memchr (sv, c, nbytes)
	void *sv;
	char c;
	int nbytes;
{
	char *s = (char *)sv;						/* Force byte pointer */

	while (nbytes-- > 0) if (*s++ == c) return (void *)(s - 1);
	return (void *)NULL;
}


/* ================================================================ */
/* Memory allocation. */

void *
c$malloc (size)
	int size;
{
	void *blk;

	if (size == 0) return NULL;				/* As required by ANSI. */
#lisp
	(gl:setq |blk.array| 
		    gl:(make-array zclib:|size| :type 'art-8b
					    :named-structure-symbol 'zeta-c:array
					    :leader-list `((nil ,zeta-c:(zctype>array-of
											    (zctype>char) zclib:|size|))
								    nil nil nil nil nil)))
	zeta-c:(setf (zcprim>art-8b-slot zclib:|blk.array|) zclib:|blk.array|)
	(gl:setq |blk.index| 0)
#endlisp
	return blk;
}

/* Calls of the form "(foo *) calloc (nelem, sizeof(foo))" are intercepted by the
   cast+ primitive so as to do proper initialization and type setup.  This is
   provided to catch cases not of that form; but note that clearing to binary
   zeros is not always going to be the right thing, depending on the type
   cast to. */
void *
c$calloc (nelem, elsize)
	int nelem, elsize;
{
	return malloc(nelem * elsize);
}

void *
c$realloc (frob, newsize)
	void *frob;
	int newsize;
{
	void *newfrob;

	if (newsize == 0) return NULL;			   /* As required by ANSI. */
	/* Want a way of making sure frob was created by malloc or calloc. */
	if ((int)@|frob.index| != 0)
		@(gl:ferror "Invalid pointer for reallocation (index must be zero)");
#lisp
	(gl:nlet ((primary-form (gl:if (gl:eq (gl:named-structure-p |frob.array|)
								   'zeta-c::cast-array)
							 (zeta-c:zcprim>array-desc |frob.array|)
						 |frob.array|))
			((old-size (gl:array-length primary-form))
			 (primary-scale (zeta-c:zcprim>array-scale primary-form))
			 (q-frob (zeta-c:zcprim>art-q-slot primary-form))
			 (16b-frob (zeta-c:zcprim>art-16b-slot primary-form))
			 (8b-frob (zeta-c:zcprim>art-8b-slot primary-form))))
	  (gl:setq |newfrob.array|
			 (gl:make-array (zeta-c:zctype>rescale-index |newsize| ':8B
												primary-scale)
						 :type (gl:array-type primary-form)
						 :named-structure-symbol
						 (gl:named-structure-p primary-form)
						 :leader-length (zeta-c:zcprim>array-leader-length)
						 :initial-element 0))
	  (zeta-c:zcprim>store-scale-slot primary-scale |newfrob.array|
							    |newfrob.array|)
	  (gl:when q-frob
	    (gl:let ((q-size (zeta-c:zctype>rescale-index old-size primary-scale ':Q))
			   (q-newfrob (zeta-c:zcprim>array-as-Q |newfrob.array|)))
		 (gl:copy-array-portion q-frob 0 q-size q-newfrob 0 q-size)))
	  (gl:when 16b-frob
	    (gl:let ((16b-newfrob (zeta-c:zcprim>array-as-16B |newfrob.array|)))
		 (gl:unless #+3600 q-frob #-3600 nil	; On A-machine, copy unboxed too.
		   (gl:copy-array-contents 16b-frob 16b-newfrob))))
	  (gl:when 8b-frob
	    (gl:let ((8b-newfrob (zeta-c:zcprim>array-as-8B |newfrob.array|)))
		 (gl:unless #+3600 (gl:or q-frob 16b-frob) #-3600 16b-frob
		   (gl:copy-array-contents 8b-frob 8b-newfrob))))
	  (gl:nlet ((type (zeta-c:zcprim>array-consed-type primary-form))
			  (env (zeta-c:zcprim>array-consed-env primary-form))
			  ((dtype (gl:and (zeta-c:zctype>array-p type)
						   (zeta-c:zctype>pointer-deref-type type)))
			   ((newtype
				 (gl:if (gl:null dtype) type
				   (zeta-c:zctype>array-of
					dtype
					(gl:/ |newsize| (zeta-c:zctype>sizeof dtype env))))))))
	    (gl:setf (zeta-c:zcprim>array-desc |newfrob.array|)
			   (gl:list gl:nil newtype env)))
	  (gl:setq |newfrob.array| (zeta-c:zcprim>array-as-scale
						    (zeta-c:zcprim>array-scale |frob.array|)
						    |newfrob.array|))
	  (gl:setq |newfrob.index| 0))
#endlisp
	/* Want a way of marking old frob as freed. */
	return newfrob;
}

/* Prevent cross-file type reference warnings. */
#lisp
  (gl:rplaca (gl:assq 'zclib:|zcclib.c| (gl:get 'c:|malloc| 'zeta-c:identifier))
		   ':predefined)
  (gl:rplaca (gl:assq 'zclib:|zcclib.c| (gl:get 'c:|calloc| 'zeta-c:identifier))
		   ':predefined)
  (gl:rplaca (gl:assq 'zclib:|zcclib.c| (gl:get 'c:|realloc| 'zeta-c:identifier))
		   ':predefined)
#endlisp




/* ================================================================ */
/* Character type table (used by macros in CTYPE.H). */

char c$_ctype_[257] = {
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
 /* '$' */   0020,   0020,   0020,   0020,
 /* '(' */   0020,   0020,   0020,   0020,
 /* ',' */   0020,   0020,   0020,   0020,
 /* '0' */   0004,   0004,   0004,   0004,
 /* '4' */   0004,   0004,   0004,   0004,
 /* '8' */   0004,   0004,   0020,   0020,
 /* '<' */   0020,   0020,   0020,   0020,
 /* '@' */   0020,   0101,   0101,   0101,
 /* 'D' */   0101,   0101,   0101,   0001,
 /* 'H' */   0001,   0001,   0001,   0001,
 /* 'L' */   0001,   0001,   0001,   0001,
 /* 'P' */   0001,   0001,   0001,   0001,
 /* 'T' */   0001,   0001,   0001,   0001,
 /* 'X' */   0001,   0001,   0001,   0020,
 /* '\' */   0020,   0020,   0020,   0020,
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

#include <ctype.h>

int
C$toupper (c)
	int c;
{
	return (islower(c) ? _toupper(c) : c);
}

int
C$tolower (c)
	int c;
{
	return (isupper(c) ? _tolower(c) : c);
}


/* ================================================================ */
/* Signals. */




/* ================================================================ */
/* Standard I/O library. */

 int C$fgetc(), C$ungetc(), _sgetc(), _scnf();

int
C$scanf (fmt, pointers)
	char *fmt;
	restarg void pointers;
{
	return _scnf ((int (*)())NULL, C$ungetc, stdin, fmt, (void *(*))&pointers);
}

int
C$sscanf (str, fmt, pointers)
	char *str, *fmt;
	restarg void pointers;
{
	return _scnf (_sgetc, (int (*)())NULL, (FILE *)&str, fmt,
			    (void *(*))&pointers);
}

int _sgetc(strp)
	char **strp;
{
	char c;

	c = *(*strp)++;
	return c ? c : EOF;
}

int
C$fscanf (stream, fmt, pointers)
	FILE *stream;
	char *fmt;
	restarg void pointers;
{
	return _scnf (C$fgetc, C$ungetc, stream, fmt, (void *(*))&pointers);
}

#lisp
; The LMITI version uses a single string always, so only one of these can be in
; effect at any point (dynamically).  The Symbolics version is for Rel 6+ only.
(gl:defmacro with-temp-indirect-string ((var string start length) gl:&body body)
  #-Symbolics
  `(gl:let ((,var temp-indirect-string))
     (gl:unwind-protect
	 (gl:progn
	   (gl:process-lock temp-indirect-string-lock gl:nil "scanf Lock")
	   (gl:%p-store-contents-offset ,string ,var 1)
	   (gl:%p-store-contents-offset ,length ,var 2)
	   (gl:%p-store-contents-offset ,start ,var 3)
	   (gl:setf (gl:fill-pointer ,var) ,length)
	   . ,body)
       (gl:process-unlock temp-indirect-string-lock gl:nil gl:nil)))
  #+Symbolics `(sys:with-stack-array (,var ,length :type gl:art-string
							   :displaced-to ,string
							   :displaced-index-offset ,start)
			  . ,body))

 #-Symbolics (gl:defvar temp-indirect-string 
				    (gl:make-array 1000 :type gl:art-string
							    :displaced-to "" :fill-pointer 0
							    :displaced-index-offset 0))
 #-Symbolics (gl:defvar temp-indirect-string-lock (gl:car-location '(gl:nil)))
(gl:comment "This has to be here because of the #-")
#endlisp

int
_scnf (getfn, ungetfn, stream, fmt, pointers)
	int (*getfn)(), (*ungetfn)();
	FILE *stream;
	char *fmt;
	void **pointers;
{
	int ic, nchars = -1, ires = 0, noassign, fieldwidth, ival;
	int idecp, ifldig, complement, i;
	char fmtc, argsize, sign, expsign, *cp, flbuf[128], scanset[256];
	double dval;
	
#define _scnfin()		(++nchars, getfn ? (*getfn) (stream) : getchar())
	ic = _scnfin();					/* read first char */
	while ((fmtc = *fmt++)) {
		if (ic == EOF) return ires ? ires : EOF;
		if (isspace(fmtc)) while (isspace(ic)) ic = _scnfin();
		else if (fmtc != '%') {
			if (ic == fmtc) ic = _scnfin();
			else {
				if (ungetfn) (*ungetfn) (ic, stream);
				return ires;
			}
		}
		else {						/* we have a % */
			noassign = FALSE;
#define DEFAULT_FIELD_WIDTH	8388607
			fieldwidth = DEFAULT_FIELD_WIDTH;
			argsize = NUL;
			if ((fmtc = *fmt++) == '*') {
				noassign = TRUE;
				fmtc = *fmt++;
			}
			if (isdigit(fmtc)) {
				fieldwidth = fmtc - '0';
				while (isdigit(fmtc = *fmt++))
					fieldwidth = 10*fieldwidth + fmtc - '0';
				if (fieldwidth == 0) ++fieldwidth;
			}
			if (fmtc == 'h'  ||  fmtc == 'l'  ||  fmtc == 'L') {
				argsize = fmtc;
				fmtc = *fmt++;
			}
			switch (fmtc) {

			case 'i':					/* ANSI */
				while (isspace(ic)) ic = _scnfin();
				if (ic == EOF) break;
				if (ic == '0') {
					ic = _scnfin();
					if (ic == 'x'  ||  ic == 'X') {
						ic = _scnfin();
						goto hex;
					}
					else goto octal;
				}
				fmtc = 'd';			/* and fall through to decimal: */
			case 'd':
			case 'u':
			decimal:
				sign = '+';
				ival = 0;
				while (isspace(ic)) ic = _scnfin();
				if (ic == EOF) { noassign = TRUE; break; }
				if (fmtc == 'd'  &&  (ic == '+'  ||  ic == '-')) {
					sign = ic;
					ic = _scnfin();
					--fieldwidth;
				}
				while (fieldwidth-- > 0  &&  isdigit(ic)) {
					ival = 10*ival + ic - '0';
					ic = _scnfin();
				}
				if (sign == '-') ival = -ival;
				break;
			case 'o':
			octal:
				ival = 0;
				while (isspace(ic)) ic = _scnfin();
				if (ic == EOF) { noassign = TRUE; break; }
				while (fieldwidth-- > 0  &&  isdigit(ic) /* &&  ic < '8' */) {
					ival = 8*ival + ic - '0';
					ic = _scnfin();
				}
				break;
			case 'x': case 'X':
			hex:
				ival = 0;
				while (isspace(ic)) ic = _scnfin();
				if (ic == EOF) { noassign = TRUE; break; }
				if (ic == '0') {
					ic = _scnfin();
					if (ic == 'x'  ||  ic == 'X') ic = _scnfin();
				}
				while (fieldwidth-- > 0  &&  isxdigit(ic)) {
					ival = 16*ival + ((ic <= '9') ? ic - '0'
									 : (ic & 0x5f) - 'A' + 10);
					ic = _scnfin();
				}
				break;
			case 'e':
			case 'f': case 'F':
			case 'g': case 'G':
				ifldig = idecp = 0;
				expsign = NUL;
				while (isspace(ic)) ic = _scnfin();
				if (ic == EOF) { noassign = TRUE; break; }
				if (ic == '+'  ||  ic == '-') {
					flbuf[ifldig++] = ic;
					ic = _scnfin();
					--fieldwidth;
				}
				while (fieldwidth-- > 0) {
					if (isdigit(ic)) ;
					else if (!idecp  &&  !expsign  &&  ic == '.') ++idecp;
					else if (!expsign  &&  (ic == 'e'  ||  ic == 'E')) {
						flbuf[ifldig++] = ic;
						ic = _scnfin();
						--fieldwidth;
						if (ic == '+'  ||  ic == '-') {
							flbuf[ifldig++] = ic;
							ic = _scnfin();
							--fieldwidth;
						}
						else expsign = '+';
					}
					else break;
					flbuf[ifldig++] = ic;
					ic = _scnfin();
					--fieldwidth;
				}
				if (!idecp  &&  !expsign) flbuf[ifldig++] = '.'; /* reader needs . */
				if (!noassign) {
					dval = (double)
						  @(with-temp-indirect-string (temp |flbuf| 0 |ifldig|)
							(si:xr-read-flonum temp #+3600 :double-float
											    #-3600 gl:nil));
					if (argsize == 'l'  ||  argsize == 'L')
						*((double *) (pointers[ires++])) = dval;
					else *((float *) (pointers[ires++])) = (float)dval;
					noassign = TRUE;
				}
				break;
			case 'c':
				if (fieldwidth = DEFAULT_FIELD_WIDTH) fieldwidth = 1;
				cp = (char *)(pointers[ires]);
				while (fieldwidth-- > 0  &&  ic != EOF) {
					if (!noassign) *cp++ = ic;
					ic = _scnfin();
				}
				if (!noassign  &&  fieldwidth == -1) ++ires;	    /* success */
				noassign = TRUE;
				break;
			case 's':
				cp = (char *)(pointers[ires]);
				while (isspace(ic)) ic = _scnfin();
				if (ic == EOF) { noassign = TRUE; break; }
				while (fieldwidth-- > 0  &&  ic != EOF  &&  !isspace(ic)) {
					if (!noassign) *cp++ = ic;
					ic = _scnfin();
				}
				if (!noassign) { *cp = NUL; ++ires; }
				noassign = TRUE;
				break;
			case '[':
				complement = FALSE;
				if ((fmtc = *fmt++) == '^') {
					if (fmtc) fmtc = *fmt++;
					complement = TRUE;
				}
				for (i = 0; i < 256; ++i) scanset[i] = complement;
				while (fmtc  &&  fmtc != ']') {
					scanset[fmtc] = !complement;
					fmtc = *fmt++;
				}
				cp = (char *)(pointers[ires++]);
				while (fieldwidth-- > 0  &&  ic != EOF  &&  scanset[ic]) {
					if (!noassign) *cp++ = ic;
					ic = _scnfin();
				}
				if (!noassign) *cp = NUL;
				noassign = TRUE;
				break;
			case 'n':					/* ANSI extension: number of chars read */
				ival = nchars;
				break;
			case 'p':					/* ANSI extension: read a pointer back in */
				@(gl:ferror "scanf %p directive not yet implemented");
				break;
			}
			if (!noassign) {
				if (2*ires >= (int)@(gl:array-length |pointers.array|))
					@(gl:ferror "Not enough result pointers to accept conversion values");
				*((int *) (pointers[ires++])) = ival;
			/* these all generate identical code in ZETA-C, since it's the
			 * type of the array part of *pointers that says what's stored
			 *	if (argsize == 'h') *((short *) (pointers[ires++])) = ival;
			 *	else if (argsize == 'l') *((long *) (pointers[ires++])) = ival;
			 *	else *((int *) (pointers[ires++])) = ival;
			 * also note no effective signed/unsigned difference */
			}
		}
	}
	return ires;
}


/* ================================================================ */
/* Emulated Unix System Calls. */

#include <sys/types.h>				/* Declares time_t. */
#include <sys/times.h>				/* Declares struct tms. */

void 
times (buffer)						/* Should return nums in 60ths of a sec */
	struct tms *buffer;
{
	buffer->tms_utime = (time_t) GL$TIME();
	buffer->tms_stime = buffer->tms_cutime = buffer->tms_cstime = 0;
}


/* End of ZCCLIB.C */
