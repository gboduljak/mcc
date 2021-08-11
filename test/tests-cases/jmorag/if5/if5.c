int cond(int b)
{
  int x;
  if (b)
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