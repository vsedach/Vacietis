// From uClibc http://www.uclibc.org/ by Erik Andersen <andersen@codepoet.org>
// LGPL 2.1 license

#define size_t int

char *strrchr( const  char *s, int c)
{
	 const char *p;

	p = NULL;
	do {
		if (*s == (char) c) {
			p = s;
		}
	} while (*s++);

	return (char *) p;			/* silence the warning */
}

size_t strspn(const char *s1, const char *s2)
{
	 const char *s = s1;
	 const char *p = s2;

	while (*p) {
		if (*p++ == *s) {
			++s;
			p = s2;
		}
	}
	return s - s1;
}

char *strpbrk(const char *s1, const char *s2)
{
	 const char *s;
	 const char *p;

	for ( s=s1 ; *s ; s++ ) {
		for ( p=s2 ; *p ; p++ ) {
			if (*p == *s) return (char *) s; /* silence the warning */
		}
	}
	return NULL;
}

char *strstr(const char *s1, const char *s2)
{
	 const char *s = s1;
	 const char *p = s2;

	do {
		if (!*p) {
			return (char *) s1;;
		}
		if (*p == *s) {
			++p;
			++s;
		} else {
			p = s2;
			if (!*s) {
				return NULL;
			}
			s = ++s1;
		}
	} while (1);
}

char *strtok(char *  s1, const char *  s2) {
	static char *next_start;	/* Initialized to 0 since in bss. */
	return strtok_r(s1, s2, &next_start);
}

char *strtok_r(char *  s1, const char *  s2, char **  next_start) {
   char *s;
   char *p;

  if (((s = s1) != NULL) || ((s = *next_start) != NULL)) {
    if (*(s += strspn(s, s2))) {
      if ((p = strpbrk(s, s2)) != NULL) {
        *p++ = 0;
      }
    } else {
      p = s = NULL;
    }
    *next_start = p;
  }
  return s;
}

// The following functions are from ZetaC

// str functions

char *strcpy (char *s1, char *s2) {
  char *s1temp = s1;

  do *s1++ = *s2; while (*s2++);
  return s1temp;
}

char *strncpy (char *s1, char *s2, int n) {
  char *s1temp = s1;

  while (--n >= 0) *s1++ = *s2 ? *s2++ : NULL;
  return s1temp;
}

char *strcat (char *s1, char *s2) {
  strcpy (s1 + strlen (s1), s2);
  return s1;
}

char *strncat (char *s1, char *s2, int n) {
  char *s1tmp;

  s1tmp = s1 + strlen (s1);     /* Remember where the original string ended */
  strncpy (s1tmp, s2, n);       /* The real work happens here */
  s1tmp[n] = 0;	                /* Must guarantee that result ends in NUL */
  return s1;
}

char *strchr (char *s, char *c) {
  do if (*s == c) return s; while (*s++);
  return NULL;
}


int strcmp (char *s1, char *s2) {
  char c1, c2;

  while (*s1 || *s2) {
    c1 = *s1++;
    c2 = *s2++;
    if (c1 < c2) return -1;
    if (c1 > c2) return 1;
  }
  return 0;
}

int strncmp (char *s1, char *s2, int n) {
  char c1, c2;

  while (n-- > 0 && (*s1 || *s2)) {	 /* Just like strcmp, but for < n chars */
    c1 = *s1++;
    c2 = *s2++;
    if (c1 < c2) return -1;
    if (c1 > c2) return 1;
  }
  return 0;
}

int strlen (char *s) {
  char *s0 = s;

  while (*s) s++;
  return s - s0;
}

int strcspn (char *s1, char *s2) {
  int n = 0;

  while (s1[n] &&              /* WHILE not at the end of S1, and */
         !strchr(s2, s1[n]))   /* not not at one of S2s chars */
    n++;
  return n;
}

// mem functions

char *memcpy (char *dest, char *src, int nbytes) {
  void *tdest = dest;
  while (nbytes-- > 0) *dest++ = *src++;
  return tdest;
}

char *memmove (char *dest, char *src, int nbytes) {
  void *tdest = dest;

  if (src > dest) while (nbytes-- > 0) *dest++ = *src++;
  else {
    src += nbytes;
    dest += nbytes;
    while (nbytes-- > 0) *--dest = *--src;
  }
  return tdest;
}

char *memset (char *dest, char c, int nbytes) {
  while (nbytes-- > 0) *dest++ = c;
  return dest;
}

void *memchr (char *s, char c, int nbytes) {
  while (nbytes-- > 0) if (*s++ == c) return (void *)(s - 1);
  return (void *)NULL;
}

int memcmp (char *m1, char *m2, int nbytes) {
  char c1, c2;

  while (nbytes-- > 0) {
    c1 = *m1++;
    c2 = *m2++;
    if (c1 < c2) return -1;
    if (c1 > c2) return 1;
  }
  return 0;
}
