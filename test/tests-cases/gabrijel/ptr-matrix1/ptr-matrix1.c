int main() {

  int n;
  int** ptr;
  int i;
  int j;
  int k;

  scanf("%d", &n);
  printf("matrix size is: %d \n", n);

  ptr = (int**)malloc(sizeof(int*) * n);

  for (i = 0; i < n; i = i+1) {
    ptr[i] = (int*)malloc(sizeof(int) * n);
  }
  
  k = 0;
  printf("k is: %d \n", k);

  i = 0;
  for (i = 0; i < n; i = i + 1) {
    for (j = 0; j < n; j = j + 1) {
      ptr[i][j] = k;
      printf("current k is: %d \n", k);
      ptr[i][j] = k;
      printf("current set k is: %d \n", ptr[i][j]);
      k = k + 1;
    }
  }

  for (i = 0; i < n; i = i + 1) {
    for (j = 0; j < n; j = j + 1) {
      printf("%d ", ptr[i][j]);
    }
    printf("\n");
  }
}