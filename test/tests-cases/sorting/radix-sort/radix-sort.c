#include "radix-sort.h"
#include "counting-sort.h"

void RadixSort(int *A, int digits, int length) {
  for (int digit = 1; digit <= digits; digit=digit+1) {
     CountingSortOnDigit(A, digit, length);
  }
}