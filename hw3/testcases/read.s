{
  read (x);
  read (y);

  while (x) {
    if (y <= x) {
      print (x);
      x = !x;
    }
    else {
      read (y);
    }
  }
}
