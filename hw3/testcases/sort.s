/* Sort */
{
  int[5] arr;
  int n;
  int tmp;
  int k;
  int j;
  int i;

  n = 5;
  i = 0;

  arr[0] = 8;
  arr[1] = 4;
  arr[2] = 5;
  arr[3] = 1;
  arr[4] = 10;
  
  /* 8 4 5 1 10 */

  do {
    j = 1;
    while(j < n - i) {
      k = j - 1;
      if (arr[k] > arr[j]) {
        tmp = arr[k];
        arr[k] = arr[j];
        arr[j] = tmp;
      }
      j++;
    }
    i++;
  }while(i < n);

  i = 0;
  while (i < n) {
    print (arr[i]);   /* 1 4 5 8 10 */
    i++;
  }
}
