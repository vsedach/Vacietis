int foobar (int x) {
  if (x < 1) {
    return 1;
  } else if (x <= 1) {
    return 2;
  } else if (x > 1) {
    return 3;
  }
}

int foobaz (int x) {
  if (x < 1)       return 4;
  else if (x <= 1) return 5;
  else             return 6;
}

int main(void) {
  int bar = 0;
  for (int i = 0; i < 3; i++) {
    bar += foobar(i) + foobaz(i);
  }
  return bar;
}
