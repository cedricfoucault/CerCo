
#define SIZE 5

signed char min (signed char tab[], signed char size, signed char n) {
  signed char i, min_index, min;

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

void swap (signed char tab[], signed char i, signed char j) {
  signed char t;
  t = tab[i] ; tab[i] = tab[j] ; tab[j] = t;
}

void bubble_sort(signed char tab[], signed char size) {
  signed char i, min_index;

  for (i = 0 ; i < size ; i++) {
    min_index = min(tab, size, i);
    swap(tab, i, min_index);
  }
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
  signed char tab[SIZE] = {26, -21, 43, -62, 8};

  bubble_sort(tab, SIZE);
  print_tab(tab, SIZE);

  return 0;
}
