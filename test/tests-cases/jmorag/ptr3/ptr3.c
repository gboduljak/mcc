int main()
{
  int i;
  int *n; int *m;
  // double *f; double *g;
  // n = (int *)malloc(10 * sizeof(int));
  // f = (double *)malloc(10 * sizeof(double));

  m = n + 9;
  // g = f + 9;
  printf("%d\n", m - n);
  // printf("%d\n", g - f);

  // printf("%d\n", (m - n == g - f));
  // free((void *)n);
  // free((void *)f);

  return 0;
}