#include "quicksort.h"
// #include <stdio.h>
// #include <stdlib.h>

int main() {
  int length;
  length = 0;

  int i;
  i = 0;

  scanf("%d", &length);
  int *A;
  A = malloc(sizeof(int) * length);

  for (i = 0; i < length; i = i + 1) {
    scanf("%d", &A[i]);
  }
  QuickSort(A, 0, length - 1);
  for (i = 0; i < length; i = i + 1) {
    printf("%d ", A[i]);
  }
  free(A);
}