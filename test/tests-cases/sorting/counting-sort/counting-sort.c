#include "counting-sort.h"

void CountingSort(int *A, int length) {
  int *B = (int *)malloc(sizeof(int) * length);
  int *C = (int *)malloc(sizeof(int) * length);
  int j = 0;

  memset(C, 0, length);

  for (j = 0; j < length; j=j+1) {
    C[A[j]]=C[A[j]]+1;
  }
  for (j = 1; j < length; j=j+1) {
    C[j] = C[j - 1] + C[j];
  }
  for (j = 0; j < length; j=j+1)
  {
    C[j]=C[j]-1;
  }
  for (j = length - 1; j >= 0; j=j-1) {
    B[C[A[j]]] = A[j];
    C[A[j]]= C[A[j]]-1;
  }
  for (j = 0; j < length; j=j+1) {
    A[j] = B[j];
  }

  free(C);
  free(B);
}