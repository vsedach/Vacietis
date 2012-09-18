(in-package #:vacietis.test.basic)
(in-readtable vacietis:vacietis)

(in-suite vacietis.test::basic-tests)

(eval-test addition0
  "1 + 2;"
  3)

(eval-test subtraction0
  "3-2;"
  1)

(eval-test global-var
  "int foobar = 10;
foobar;"
  10)

;; (eval-test for-loop0
;;   "int foobar;
;; for (int x = 0, foobar = 0; x <= 10; x++) foobar += x;
;; foobar;"
;;   55) ;; comes out to 0 because of foobar scope, bug or feature?

(eval-test for-loop1
  "int foobar = 0;
for (int x = 0; x <= 10; x++) foobar += x;
foobar;"
  55)

(eval-test string-literal
  "char foobar[] = \"foobar\";
foobar;"
  "foobar")

(eval-test h&s-while-string-copy
  "char source_pointer[] = \"foobar\", dest_pointer[7];
while ( *dest_pointer++ = *source_pointer++ );
dest_pointer - 7;"
  "foobar")

(eval-test define-foo
  "#define FOO 1
int x = FOO;
x;"
  1)

(eval-test define-foo1
  "#define foo 2
int baz = foo * 2;
baz;"
  4)

(eval-test define-foo2
  "#define foo 1 + 4
int baz = foo * 2;
baz;"
  9)

(eval-test preprocessor-if-1
  "#if 2 < 1
int baz = 5;
baz;
#endif"
  nil)

(eval-test preprocessor-if-2
  "int baz = 123;
#if 2 >= 1
baz = 456;
#endif
baz;"
  456)

(eval-test preprocessor-ifdef
  "#define FOOMAX
int baz = 1;
#ifdef FOOMAX
int baz = 2;
#endif
baz;"
  2)

(eval-test preprocessor-define-template
  "#define foo(x, y) x+y
foo(1,2);"
  3)

(eval-test sizeof-static-array
  "static char buf[10];
sizeof buf;"
  10)

(eval-test sizeof-int
  "int foo;
sizeof foo;"
  1)

(eval-test sizeof-int1
  "int foo1 = 120;
sizeof foo1;"
  1)

(eval-test sizeof0
  "char foobar;
sizeof (foobar);"
  1)

(eval-test sizeof1
  "long foobar;
1 + sizeof (foobar);"
  2)

(eval-test sizeof2
  "sizeof int;"
  1)

(eval-test sizeof3
  "sizeof (int);"
  1)

(eval-test if-then-else1
  "int baz;
if (2 < 1) {
  baz = 2;
} else {
  baz = 3;
}
baz;"
  3)

(eval-test if-then-none
  "int baz = 0;
if (2 < 1) {
  baz = 2;
}
baz;"
  0)

(eval-test do-while1
  "int foo = 0;
do foo++; while (foo < 1);
foo;"
  1)

(eval-test setf-aref
  "int foo[3];
foo[0] = 123;
foo[0];"
  123)

(eval-test strlength1
  "#include <string.h>
strlen(\"foobar\");"
  6)

(eval-test reverse
  "void reverse(char *str) {
  char * end = str;
  char tmp;

  if (str) {
    while (*end) {
      ++end;
    }

    --end;

    while (str < end) {
      tmp = *str;
      *str++ = *end;
      *end-- = tmp;
    }
  }
}

char *foo = \"foobar\";
reverse(foo);
foo;"
  "raboof")

(eval-test sprintf-padchar
  "#include <stdio.h>
char *foo[6];
sprintf(foo, \"%-5c\", 'X');"
  "X    ")

(eval-test typedef
  "typedef int Baz;
Baz baz = 4;
baz;"
  4)

(eval-test define-define
  "#define FOO 1
#define BAR FOO
BAR;"
  1)

(eval-test define-define1
  "#define fo0 (x, y) x >> y
#define Bar fo0
Bar(0xFFF, 2);"
  1023)

(eval-test function-pointer1
  "int add(int *x, int *y) {
  return *x + *y;
}

int apply(int ((*fun))(int *, int *), int x, int y) {
  return (*fun)(&x, &y);
}

apply((int (*)(int *, int *)) add, 2, 3);"
  5)

(eval-test simple-function1
  "void foo(int a, int b) {
return a + b;
}
foo(11, 13);"
  24)

(eval-test function0
  "int max(int a, int b)
{
return a > b ? a : b;
}
max(-3, 10);"
  10)

(eval-test function1
  "extern int max(int a, int b)
{
return a > b ? a : b;
}
max(234, 0);"
  234)

(eval-test no-arg-function
  "int a = -4, b = 7;
void foo() {
return a + b;
}
foo();"
  3)

(eval-test labeled-statement1
  "void foo() {
int a = 2, b = 5;
int c = 3;
goto baz;
c = 7;
baz:
return a + b + c;
}
foo();"
  10)

(eval-test h&s-while1
  "int pow(int base, int exponent)
{
    int result = 1;
    while (exponent > 0) {
        if ( exponent % 2 ) result *= base;
        base *= base;
        exponent /= 2;
    }
    return result;
}
pow(3, 4);"
  81)

(eval-test enums
  "enum foo { bar, baz };
enum foo x = bar;
enum foo y = baz;
int A = 0;

if (x == bar) A = 3;
A;"
  3)

(eval-test enums1
  "enum foo { bar, baz } x = bar, y = baz;
int A = 0;

if (x == bar) A = 3;
A;"
  3)
