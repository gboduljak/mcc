#include "insertion-sort.h"
// #include <stdio.h>
// #include <stdlib.h>

int main() {
  int length;
  length = 0;

  scanf("%d", &length);
  int i;
  i = 0;

  int *A;
  A = (int *)malloc(sizeof(int) * length);

  for (i = 0; i < length; i = i + 1) {
    scanf("%d", &A[i]);
  }
  InsertionSort(A, length);
  for (i = 0; i < length; i = i + 1) {
    printf("%d ", A[i]);
  }
  free((void *)A);
}