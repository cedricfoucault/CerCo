
#define SIZE 5

int min (int tab[], int size, int n) {
  int i, min_index, min;

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

void swap (int tab[], int i, int j) {
  int t;
  t = tab[i] ; tab[i] = tab[j] ; tab[j] = t;
}

void bubble_sort(int tab[], int size) {
  int i, min_index;

  for (i = 0 ; i < size ; i++) {
    min_index = min(tab, size, i);
    swap(tab, i, min_index);
  }
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
  int tab[SIZE] = {26, -21, 43, -62, 8};

  bubble_sort(tab, SIZE);
  print_tab(tab, SIZE);

  return 0;
}
