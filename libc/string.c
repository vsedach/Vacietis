// From uClibc http://www.uclibc.org/ by Erik Andersen <andersen@codepoet.org>
// LGPL 2.1 license

#define size_t int

char *strcpy(char *s1, const char *s2) {
  char *s = s1;
  while ( (*s++ = *s2++) != 0 );
  return s1;
}

char *strncpy(char *s1, const char *s2, size_t n) {
	char *s = s1;

	while (n) {
		if ((*s = *s2) != 0) s2++; /* Need to fill tail with 0s. */
		++s;
		--n;
	}

	return s1;
}

char *strcat(char *  s1,  const char *  s2)
{
	 char *s = s1;

	while (*s++);
	--s;
	while ((*s++ = *s2++) != 0);

	return s1;
}

char *strncat(char *  s1,  const char *  s2,
				size_t n)
{
	 char *s = s1;

	while (*s++);
	--s;
	while (n && ((*s = *s2++) != 0)) {
		--n;
		++s;
	}
	*s = 0;

	return s1;
}

int strcmp( const char *s1,  const char *s2)
{

	int r;

	while (((r = ((int)(*((uchar *)s1))) - *((uchar *)s2++))
			== 0) && *s1++);

	return r;
}

// yup, this is real code, formatting and all
int strncmp( const char *s1,  const char *s2, size_t n)
{
	int r = 0;

	while (n--
		   && ((r = ((int)(*((unsigned char *)s1))) - *((unsigned char *)s2++))
			== 0)
		   && *s1++);

	return r;
}

char *strchr( const char *s, int c)
{
	do {
		if (*s == ((char)c)) {
			return (char *) s;	/* silence the warning */
		}
	} while (*s++);

	return NULL;
}

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

size_t strlen(const char *s) {
	const char *p;
	for (p=s ; *p ; p++);
	return p - s;
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
