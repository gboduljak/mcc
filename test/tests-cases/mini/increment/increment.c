int main() {
  int i;
  int j;
  double k;

  i = 0;
  i++;
  i++;

  for (i = 0; i < 10; i++) {
    printf("%d\n", i);
  }
  i--;
  printf("%d\n", i);

  k = 0.0;
  for (i = 0; i < 10; i++) {
    printf("%f\n", k++);
  }
  k--;
  printf("%f\n", k);

  int* ptr;
  ptr = &j;
  ptr--;
  
  for (i = 0; i < 20; i++) {
    ptr++;
    printf("%d\n", (int)ptr);
  }
  printf("ptr decr\n");

  for (i = 0; i < 3; i++) {
    ptr--;
    printf("%d\n", (int)ptr);
  }

}