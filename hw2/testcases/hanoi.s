/* Tower of Hanoi with 4 tokens */
{
  int x;
  int y;    /* number of tokens */
  int i;

  i = 1;
  x = 2;
  y = 4;

  while (i < y) {
    x = x * 2;
    i++;
  }
  print (x - 1);  /* 15 */
}
