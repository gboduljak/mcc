#include "counting-sort.h"

int pow(int x, int y) {
  int power;
  power = 1;
  while (y > 0) {
    power = power * x;
    y = y - 1;
  }
  return power;
}

int Digit(int num, int d) {
  int power;
  int prev_power;

  power = pow(10, d);
  prev_power = pow(10, d - 1);
  return (num - (num / power) * power) / prev_power;
}

void CountingSortOnDigit(int *A, int d, int length) {
  int *B;
  int *C;
  int i;
  int j;
  B = (int *)malloc(sizeof(int) * length);
  C = (int *)malloc(sizeof(int) * (10));
  i = 0;
  j = 0;

  for (j = 0; j < 10; j = j + 1) {
    C[j] = 0;
  }
  for (j = 0; j < length; j = j + 1) {
    C[Digit(A[j], d)] = C[Digit(A[j], d)] + 1;
  }
  for (j = 1; j < 10; j = j + 1) {
    C[j] = C[j - 1] + C[j];
  }
  for (j = 0; j < 10; j = j + 1) {
    C[j] = C[j] - 1;
  }

  for (j = length - 1; j >= 0; j = j - 1) {
    B[C[Digit(A[j], d)]] = A[j];
    C[Digit(A[j], d)] = C[Digit(A[j], d)] - 1;
  }

  for (i = 0; i < length; i = i + 1) {
    A[i] = B[i];
  }

  free((void *)C);
  free((void *)B);
}