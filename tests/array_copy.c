
#define SIZE 5

int tab1[SIZE] = {10, -3, 25, 56, -32};

void copy (int dst[], int src[], int size) {
  int i;

  for (i = 0 ; i < size ; i++)
    dst[i] = src[i];
}

void print_tab (int tab[], int size) {
  int i;

  for (i = 0 ; i < size ; i++) {
    print_sint(tab[i]);
    space();
  }
  newline();
}

int main () {
  int tab2[SIZE];
  int tab3[SIZE] = {0, 1, 2, 3, 4};

  copy(tab2, tab1, SIZE);
  copy(tab1, tab3, SIZE);

  print_tab(tab1, SIZE);
  print_tab(tab2, SIZE);
  print_tab(tab3, SIZE);

  return 0;
}
