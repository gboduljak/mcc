
int a;
int b;

void printa()
{
  printf("%d\n", a);
}

void printbb()
{
  printf("%d\n", b);
}

void incab()
{
  a = a + 1;
  b = b + 1;
}

int main()
{
  a = 42;
  b = 21;
  printa();
  printbb();
  incab();
  printa();
  printbb();
  return 0;
}