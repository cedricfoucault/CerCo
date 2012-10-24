
typedef struct foo {
  int dummy;
  int x;
} foo;

foo *p;
foo save;

int fact1 (int x) {
  if (x <= 1) return 1;
  return (x * fact1(x-1));
}

int fact2 (int x) {
  int i, res = 1;

  for (i = 1 ; i <= x ; i++)
    res *= i;

  return res;
}

int main () {
  foo x, y;
  foo* q[3];

  x.x = 5;

  q[1] = &x;
  p = &x;
  save = x;
  x.x = fact1(save.x);
  y.x = fact2(save.x);

  print_sint((*(q[1])).x == y.x);
  newline();

  return 0;
}
