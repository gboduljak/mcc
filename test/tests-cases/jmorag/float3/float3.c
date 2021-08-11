void testfloat(double a, double b)
{
  printf("%f\n", a + b);
  printf("%f\n", a - b);
  printf("%f\n", a * b);
  printf("%f\n", a / b);
  printf("%d\n", a == b);
  printf("%d\n", a == a);
  printf("%d\n", a != b);
  printf("%d\n", a != a);
  printf("%d\n", a > b);
  printf("%d\n", a >= b);
  printf("%d\n", a < b);
  printf("%d\n", a <= b);
}

int main()
{
  double c;
  double d;

  c = 42.0;
  d = 3.14159;

  testfloat(c, d);
  testfloat(d, d);

  return 0;
}