{
  int x;
  int y;
  x = 2;
  y = 2;
  while (x >= 0) {
    if (x && y) print (1);
    else print(2);

    if (y && x) print (3);
    else print (4);

    if (x || y) print (5);
    else print (6);

    if (y || x) print (7);
    else print (8);

    x = x - 1;
    y = 0;
  }
  /* 1 3 5 7 2 4 5 7 2 4 6 8 */
}
