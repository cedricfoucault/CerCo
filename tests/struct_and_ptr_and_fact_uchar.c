
typedef struct foo {
  unsigned char dummy;
  unsigned char x;
} foo;

foo *p;
foo save;

unsigned char fact1 (unsigned char x) {
  if (x <= 1) return 1;
  return (x * fact1(x-1));
}

unsigned char fact2 (unsigned char x) {
  unsigned char i, res = 1;

  for (i = 1 ; i <= x ; i++)
    res *= i;

  return res;
}

unsigned char main () {
  foo x, y;
  foo* q[3];

  x.x = 5;

  q[1] = &x;
  p = &x;
  save = x;
  x.x = fact1(save.x);
  y.x = fact2(save.x);

  print_uchar((*(q[1])).x == y.x);
  newline();

  return 0;
}
