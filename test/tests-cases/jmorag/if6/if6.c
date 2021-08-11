int cond(int b)
{
  int x;
  x = 10;
  if (b)
    if (x == 10)
      x = 42;
  else
    x = 17;
  return x;
}

int main()
{
 printf("%d\n", cond(1));
 printf("%d\n", cond(0));
 return 0;
}