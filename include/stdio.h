typedef T FILE;

#define EOF -1

/* these constants are important but here defined arbitrarily */
#define FILENAME_MAX 1024
#define FOPEN_MAX 99999
#define BUFSIZ 512

#define getchar()   getc(stdin)
#define getc(f)     fgetc(f)
#define putc(c, f)  fputc(c, f)
#define putchar(c)  putc(c, stdout)
#define puts(str)   fputs(str, stdout)

