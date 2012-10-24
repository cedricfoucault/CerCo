
#define SIZE 5

void swap (unsigned char a[], unsigned char i, unsigned char j) {
  unsigned char t;
  t = a[i] ; a[i] = a[j] ; a[j] = t;
}

unsigned char partition (unsigned char a[], unsigned char l, unsigned char r) {
   unsigned char pivot, i, j;
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

void quicksort (unsigned char a[], unsigned char l, unsigned char r) {
   unsigned char j;

   if (l < r) {
     j = partition(a, l, r);
     quicksort(a, l, j-1);
     quicksort(a, j+1, r);
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

  quicksort(tab, 0, SIZE-1);
  print_tab(tab, SIZE);

  return 0;
}
