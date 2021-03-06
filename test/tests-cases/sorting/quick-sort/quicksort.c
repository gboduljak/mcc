#include "quicksort.h"

void Swap(int *A, int i, int j) {
  int temp;
  temp = A[i];
  A[i] = A[j];
  A[j] = temp;
}

int Partition(int *A, int p, int r) {
  int x;
  x = A[r];

  int i;
  i = p - 1;

  int j;
  j = 0;

  for (j = p; j < r; j = j + 1) {
    if (A[j] <= x) {
      i = i + 1;
      Swap(A, i, j);
    }
  }

  i = i + 1;
  Swap(A, i, r);
  return i;
}

void QuickSort(int *A, int p, int r) {
  if (p >= r) {
    return;
  }
  int q;
  q = Partition(A, p, r);

  QuickSort(A, p, q - 1);
  QuickSort(A, q + 1, r);
}