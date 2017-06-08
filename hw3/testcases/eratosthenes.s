/* Sieve of Eratosthenes */
{
  int[30] arr;    /*To find prime number less than 30 */
  int a;
  int b;

  a = 0;

  while (a < 30) {
    arr[a] = 1;
    a++;
  }

  a = 2;
  while (a < 30) {
    b = a * 2;
    while (b < 30) {
      arr[b] = 0;
      b = a + b;
    }
    a++;
  }

  a = 2;
  do {
    if (arr[a] == 1) {
      print (a);    /* Print out the prime numbers less than 30*/
    }
    a++;
  } while (a < 30);
  /* 2 3 5 7 11 13 17 19 23 29 */
}
