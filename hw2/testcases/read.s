/* Combination nCr*/
{
  int x;
  int y;
  
  read (x);
  read (y);

  while (x) {
    if (x > y) {
      print (x);
      x = !x;
    }
    else read (y);
  }
}
