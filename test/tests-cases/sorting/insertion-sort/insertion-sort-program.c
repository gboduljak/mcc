#include "insertion-sort.h"

int main() {
  int length = 0;
  scanf("%d", &length);
  int i = 0;
  int *A = malloc(sizeof(int) * length);
  for (i = 0; i < length; i=i+1) {
    scanf("%d", &A[i]);
  }
  InsertionSort(A, length);
  for (i = 0; i < length; i=i+1) {
    printf("%d ", A[i]);
  }
  free(A);
}