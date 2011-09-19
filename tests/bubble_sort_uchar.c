
#define SIZE 5

unsigned char min (unsigned char tab[], unsigned char size, unsigned char n) {
  unsigned char i, min_index, min;

  if (size == 0) return 0;

  min_index = n;
  min = tab[min_index];
  for (i = n+1 ; i < size ; i++) {
    if (tab[i] < min) {
      min_index = i;
      min = tab[min_index];
    }
  }

  return min_index;
}

void swap (unsigned char tab[], unsigned char i, unsigned char j) {
  unsigned char t;
  t = tab[i] ; tab[i] = tab[j] ; tab[j] = t;
}

void bubble_sort(unsigned char tab[], unsigned char size) {
  unsigned char i, min_index;

  for (i = 0 ; i < size ; i++) {
    min_index = min(tab, size, i);
    swap(tab, i, min_index);
  }
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
  unsigned char tab[SIZE] = {26, -21, 43, -62, 8};

  bubble_sort(tab, SIZE);
  print_tab(tab, SIZE);

  return 0;
}
