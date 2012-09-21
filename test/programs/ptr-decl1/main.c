#include <stdio.h>

int main () {
  int x, *y = &x;
  x = 3;
  printf("%d\n", x * *y);
}
