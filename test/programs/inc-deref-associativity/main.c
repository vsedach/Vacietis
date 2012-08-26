#include <stdio.h>

void main() {
  char *foo = "123";
  putchar(*foo++);
  putchar(*foo);
  putchar(*++foo);
}
