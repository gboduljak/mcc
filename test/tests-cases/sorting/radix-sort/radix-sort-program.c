#include "counting-sort.c"
#include "radix-sort.h"
// #include <stdio.h>
// #include <stdlib.h>

int main() {
  int length;
  length = 0;

  int digits;
  digits = 0;

  int i;
  i = 0;

  scanf("%d %d", &length, &digits);

  int *A;
  A = (int *)malloc(sizeof(int) * length);

  for (i = 0; i < length; i = i + 1) {
    scanf("%d", &A[i]);
  }

  RadixSort(A, digits, length);

  for (i = 0; i < length; i = i + 1) {
    printf("%d ", A[i]);
  }

  free((void *)A);
}