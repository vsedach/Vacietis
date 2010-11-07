/* -*- Mode: C; Tab-width: 5; Base: 10 -*-

	Copyright (C) 1985 by ZETA-SOFT, Ltd.
	All rights reserved.

 STRING.H for ZETA-C: declarations for string processing functions.
 Note this file does not have a package specification, as it will be
 read into (potentially) many different packages.

 This is identical to ZETA-C:Include;STRINGS.H.  */

extern char *strcat(), *strchr(), *strcpy(), *strncat(), *strncpy();
extern char *strpbrk(), *strrchr(), *strrpbrk();
extern int strcmp(), strlen(), strcspn(), strncmp(), strpos();
extern int strrpos(), strspn();
extern void *memcpy(), *memmove(), *memset(), *memchr();
extern int memcmp();
extern lispval zclib$str2lisp (char *s);

/* End of STRING.H */
