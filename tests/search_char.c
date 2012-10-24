#define SIZE 5
#define NB_FINDS 2

// Searching for the values below
signed char to_find[NB_FINDS] = {57, -1};

signed char search (signed char tab[], signed char size, signed char to_find) {
  signed char low = 0, high = size-1, i;

  while (high >= low) {
    i = (high+low) / 2;
    if (tab[i] == to_find) return i;
    if (tab[i] > to_find) high = i-1;
    if (tab[i] < to_find) low = i+1;
  }

  return (-1);
}

signed char main () {
  signed char tab[SIZE] = {-30, -18, 23, 57, 120};
  signed char res;
  signed char i;

  for (i = 0 ; i < NB_FINDS ; i++) {
    print_schar(search(tab, SIZE, to_find[i]));
    space();
  }
  newline();

  return 0;
}
