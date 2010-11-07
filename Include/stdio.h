/*
 STDIO.H for ZETA-C: sets up the default I/O environment.
 Note this file does not have a package specification, as it will be
 read into (potentially) many different packages. */

#define FILE		lispval
#define NULL		((void *) 0)
#define EOF		(-1)

#define BUFSIZ		512		/* There's nothing this actually corresponds to */

extern FILE *stdin, *stdout, *stderr;
#lisp
  ;; We don't want this file to "own" these
  (zeta-c:zclib>initialize-file-pointer |stdin| 0)
  (zeta-c:zclib>initialize-file-pointer |stdout| 1)
  (zeta-c:zclib>initialize-file-pointer |stderr| 2)
#endlisp

extern int getc();
extern int putc(), putchar();

#define getchar()		getc(stdin)

extern FILE *fopen(), *freopen(), *fdopen();
long ftell();
char *gets(), *fgets();

#define ZETA_C

/* End of STDIO.H */
