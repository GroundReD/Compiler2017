/* Combination nCr*/
{
  int n;
  int r;
  int i;
  int tmp;
  int rfact;
  n = 5;
  r = 2;
  i = 1;
  rfact = 1;
  
  tmp = n;
  i = n - 1;
  do {    /* n! */
    n = n * i;
    i = i - 1;
  } while( i > 0);

  tmp = tmp - r;
  i = tmp - 1;
  while (i > 0) { /* (n-r)! */
    tmp = tmp * i;
    i = i - 1;
  }

  i = 1;
  while (i <= r) {  /* r! */
    rfact = rfact * i;
    i++;
  }

  print (n / (tmp * rfact));  /* 5C2 = 10 */
}
