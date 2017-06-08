/* Euclidean Algorithm */
{
  int x;
  int y;
  int i;
  int a;
  int b;
  int tmp;

  x = 128;
  y = 48;
  a = x;
  b = y;

  while(y > 0)
  {
    tmp = y;
    y = x - (x / y) * y;  /* modulus */
    x = tmp;
  }

  print(x);     /* GCD: 16 */
  print(a * b / x);   /* LCM: 384 */ 
}
