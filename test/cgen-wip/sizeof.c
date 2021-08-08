int main() {
  int size;
  int size2;
  size = sizeof(int[5][15]);
  printf("%d\n", size);
  printf("%d\n", sizeof(&size));
}