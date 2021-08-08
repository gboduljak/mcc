int foo(int a, int* b)
{
  int c;
  double d;

  c = a;

  return c + 10;
}

int main() {
 int b;
 b = 10;
 printf("%d\n", foo(37, &b));
 return 0;
}