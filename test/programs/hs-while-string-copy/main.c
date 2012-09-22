#include <stdio.h>

void main () {
  char source_array[] = "foobar", dest_array[7];
  char *source_pointer = source_array, *dest_pointer = dest_array;
  while ( *dest_pointer++ = *source_pointer++ );
  printf("%s\n", dest_pointer - 7);
}
