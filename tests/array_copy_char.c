
#define SIZE 5

signed char tab1[SIZE] = {10, -3, 25, 56, -32};

void copy (signed char dst[], signed char src[], signed char size) {
  signed char i;

  for (i = 0 ; i < size ; i++)
    dst[i] = src[i];
}

void print_tab (signed char tab[], signed char size) {
  signed char i;

  for (i = 0 ; i < size ; i++) {
    print_schar(tab[i]);
    space();
  }
  newline();
}

signed char main () {
  signed char tab2[SIZE];
  signed char tab3[SIZE] = {0, 1, 2, 3, 4};

  copy(tab2, tab1, SIZE);
  copy(tab1, tab3, SIZE);

  print_tab(tab1, SIZE);
  print_tab(tab2, SIZE);
  print_tab(tab3, SIZE);

  return 0;
}
