#include "insertion-sort.h"

void InsertionSort(int *A, int length) {
  int i;
  int j;
  int key;
  i = 0;
  j = 0;
  key = 0;

  for (j = 1; j < length; j = j + 1) {
    key = A[j];
    i = j - 1;
    while (i > -1 && A[i] > key) {
      A[i + 1] = A[i];
      i = i - 1;
    }
    A[i + 1] = key;
  }
}