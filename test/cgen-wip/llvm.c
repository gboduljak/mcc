double f(double x) {
  printf("%f", x);
  return x;
}

int main() {
  double g;
  g = 12.12;
  double res;
  res = f(g);
  printf("%f\n", res);
  return 0;
}