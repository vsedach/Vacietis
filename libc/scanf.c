#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>

// adapted from ZetaC

#define __initrest(pointers) va_list pointers; va_start(pointers, -1)


int scanf (char *fmt, ...) {
  __initrest(places);
  return _scnf ((int (*)())NULL, ungetc, stdin, fmt, places);
}

int _sgetc(char **strp) {
  char c;
  c = *(*strp)++;
  return c ? c : EOF;
}

int sscanf (char *str, char *fmt, ...) {
  __initrest(places);
  return _scnf (_sgetc, (int (*)())NULL, (FILE *)&str, fmt, places);
}

int fscanf (FILE *stream, char *fmt, ...) {
  __initrest(places);
  return _scnf (fgetc, ungetc, stream, fmt, places);
}


int
_scnf (int (*getfn)(), int (*ungetfn)(), FILE *stream, char *fmt, va_list pointers)
{
  int nchars = -1, ires = 0, noassign, fieldwidth, ival;
  int idecp, ifldig, complement, i;
  char fmtc, argsize, sign, expsign, *cp, flbuf[128], scanset[256];

#define _scnfin()             (++nchars, getfn ? (*getfn) (stream) : getchar())
#define DEFAULT_FIELD_WIDTH   8388607
#define TRUE                  1
#define FALSE                 0

  int ic = _scnfin();

  while ((fmtc = *fmt++)) {
    if (ic == EOF) return ires ? ires : EOF;

    if (isspace(fmtc)) {
      while (isspace(ic)) ic = _scnfin();
    }

    else if (fmtc != '%') {
      if (ic == fmtc) ic = _scnfin();
      else {
        if (ungetfn) (*ungetfn) (ic, stream);
        return ires;
      }
    }

    else {						/* we have a % */
      noassign = FALSE;

      fieldwidth = DEFAULT_FIELD_WIDTH;
      argsize = NULL;
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
      case 'i':
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
        expsign = NULL;
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
          flbuf[ifldig] = '\0';
          *va_arg(pointers, double) = atof(flbuf);
          ires++;
          noassign = TRUE;
        }
        break;
      case 'c':
        if (fieldwidth = DEFAULT_FIELD_WIDTH) fieldwidth = 1;
        cp = (char *)va_arg(pointers, (char *));
        while (fieldwidth-- > 0  &&  ic != EOF) {
          if (!noassign) *cp++ = ic;
          ic = _scnfin();
        }
        if (!noassign  &&  fieldwidth == -1) ++ires;	    /* success */
        noassign = TRUE;
        break;
      case 's':
        cp = (char *)va_arg(pointers, (char *));
        while (isspace(ic)) ic = _scnfin();
        if (ic == EOF) { noassign = TRUE; break; }
        while (fieldwidth-- > 0  &&  ic != EOF  &&  !isspace(ic)) {
          if (!noassign) *cp++ = ic;
          ic = _scnfin();
        }
        if (!noassign) { *cp = NULL; ++ires; }
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
        cp = (char *)va_arg(pointers, (char *));
        ires++;
        while (fieldwidth-- > 0  &&  ic != EOF  &&  scanset[ic]) {
          if (!noassign) *cp++ = ic;
          ic = _scnfin();
        }
        if (!noassign) *cp = NULL;
        noassign = TRUE;
        break;
      case 'n':			/* ANSI extension: number of chars read */
        ival = nchars;
        break;
      case 'p':			/* read a pointer back in, not supported */
        errno = EINVAL;
        return -1;
      }

      if (!noassign) {
        *va_arg(pointers, int) = ival;
        ires++;
      }
    }
  }
  return ires;
}
