#include <stdio.h>

int main() {
  int foo[3];
  int *x = &foo[1];

  foo[1] = 3;
  foo[2] = 5;

  int y = foo[1];

  printf("%d\n", *(x + 1));
}
