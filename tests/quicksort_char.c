
#define SIZE 5

void swap (signed char a[], signed char i, signed char j) {
  signed char t;
  t = a[i] ; a[i] = a[j] ; a[j] = t;
}

signed char partition (signed char a[], signed char l, signed char r) {
   signed char pivot, i, j;
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

void quicksort (signed char a[], signed char l, signed char r) {
   signed char j;

   if (l < r) {
     j = partition(a, l, r);
     quicksort(a, l, j-1);
     quicksort(a, j+1, r);
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

  quicksort(tab, 0, SIZE-1);
  print_tab(tab, SIZE);

  return 0;
}
