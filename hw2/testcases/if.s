/* test if case */
{
  int x;
  int i;
  i = 3;

  while (i > 0) {
    x = i;

    if (x == 1 || x == 2) 
      if (x == 1)
        print (x);
      else
        print (x+3);
    else
      print (x+4);

    i = i - 1;
  }
  /* Answer: 7, 5, 1 */
}
