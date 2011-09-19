
typedef struct foo {
  signed char dummy;
  signed char x;
} foo;

foo *p;
foo save;

signed char fact1 (signed char x) {
  if (x <= 1) return 1;
  return (x * fact1(x-1));
}

signed char fact2 (signed char x) {
  signed char i, res = 1;

  for (i = 1 ; i <= x ; i++)
    res *= i;

  return res;
}

signed char main () {
  foo x, y;
  foo* q[3];

  x.x = 5;

  q[1] = &x;
  p = &x;
  save = x;
  x.x = fact1(save.x);
  y.x = fact2(save.x);

  print_schar((*(q[1])).x == y.x);
  newline();

  return 0;
}
