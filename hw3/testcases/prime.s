/* get prime number */
{
  int num;
  int prime;
  int i;

  prime = 0;        /* is it prime number? */
  i = 2;
  num = 13;
  /*
    13 is prime number
  */

  do {
    if (num - (num / i)  * i == 0)
      prime++;
    i++;
  } while (i < num);

  if (prime == 0) 
    print(0);     /* if the number is prime number */
  else
    print(1);
  
  /* 0 */
}
