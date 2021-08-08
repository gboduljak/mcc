double f(double x) {
  printf("%f", x);
  return x;
}

double h() {
  double ret;
  char c;

  ret = 10.0;
  return ret;
}

int main() {
  double g;
  g = 12.12;
  double res;
  res = f(g);
  printf("%f\n", res);
  printf("%f\n", h());
  return 0;
}