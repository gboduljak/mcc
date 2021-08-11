void foo() {}

int bar(int a, int b, int c) { return a + c; }

int main()
{
  printf("%d\n", bar(17, 0, 25));
  return 0;
}