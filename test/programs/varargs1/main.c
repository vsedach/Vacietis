#include <stdio.h>
#include <stdarg.h>

int varsum(int num, ...) {
  int result = 0;
  va_list numbers;
  va_start(numbers, num);

  while (num--) {
    result += va_arg(numbers, int);
  }

  va_end(numbers);
  return result;
}

int main () {
  printf("Sum is %d", varsum(3, 3, 5, 7));
  return varsum(0) + 1;
}
