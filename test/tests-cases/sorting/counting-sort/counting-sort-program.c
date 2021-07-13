#include "counting-sort.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
  int length = 0;
  int i = 0;
  scanf("%d", &length);
  int *A = malloc(sizeof(int) * length);
  for (i = 0; i < length; i=i+1) {
    scanf("%d", &A[i]);
  } 
  CountingSort(A, length);
  for (i = 0; i < length; i=i+1) {
    printf("%d ", A[i]);
  } 
  free(A);
}