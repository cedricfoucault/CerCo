
#define SIZE 5

unsigned char tab1[SIZE] = {10, -3, 25, 56, -32};

void copy (unsigned char dst[], unsigned char src[], unsigned char size) {
  unsigned char i;

  for (i = 0 ; i < size ; i++)
    dst[i] = src[i];
}

void print_tab (unsigned char tab[], unsigned char size) {
  unsigned char i;

  for (i = 0 ; i < size ; i++) {
    print_uchar(tab[i]);
    space();
  }
  newline();
}

unsigned char main () {
  unsigned char tab2[SIZE];
  unsigned char tab3[SIZE] = {0, 1, 2, 3, 4};

  copy(tab2, tab1, SIZE);
  copy(tab1, tab3, SIZE);

  print_tab(tab1, SIZE);
  print_tab(tab2, SIZE);
  print_tab(tab3, SIZE);

  return 0;
}
