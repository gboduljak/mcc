#include "radix-sort.h"

int main() {
  int length = 0;
  int digits = 0;
  int i = 0;

  scanf("%d %d", &length, &digits);

  int *A = malloc(sizeof(int) * length);
  for (i = 0; i < length; i=i+1) {
    scanf("%d", &A[i]);
  }

  RadixSort(A, digits, length);

  for (i = 0; i < length; i=i+1) {
    printf("%d ", A[i]);
  }

  free(A);
}