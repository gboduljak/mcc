int main() {
  int array[10];
  int* ptr;
  int i;
  int sum1;
  int sum2;

  sum1 = 0;
  sum2 = 0;

  ptr = (int*)malloc(sizeof(int) * 10);

  for (i = 0; i < 10; i = i + 1) {
    array[i] = i + 1;
    ptr[i] = i + 1;
  }

  for (i = 0; i < 10; i = i + 1) {
    sum1 = sum1 + array[i];
    sum2 = sum2 + ptr[i];
  }

  if (sum1 == sum2) {
    printf("great success\n");
  }
  else {
    printf("array neq ptr :(");
  }

}