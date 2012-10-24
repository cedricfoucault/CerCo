
#define SIZE 5

void swap (int a[], int i, int j) {
  int t;
  t = a[i] ; a[i] = a[j] ; a[j] = t;
}

int partition (int a[], int l, int r) {
   int pivot, i, j;
   pivot = a[l];
   i = l; j = r+1;

   while (1) {
     while (i <= r && a[i] <= pivot) ++i;
     do --j; while (a[j] > pivot);
     if (i >= j) break;
     swap(a, i, j);
   }
   swap(a, l, j);
   return j;
}

void quicksort (int a[], int l, int r) {
   int j;

   if (l < r) {
     j = partition(a, l, r);
     quicksort(a, l, j-1);
     quicksort(a, j+1, r);
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

  quicksort(tab, 0, SIZE-1);
  print_tab(tab, SIZE);

  return 0;
}
