int infinity = 100000000;

void Merge(int *A, int p, int q, int r) {
  int n1 = q - p + 1;
  int n2 = r - q;

  int *L = malloc(sizeof(int) * (n1 + 1));
  int *R = malloc(sizeof(int) * (n2 + 1));

  int i = 0;
  int j = 0;
  int k = 0;

  for (i = 0; i < n1; i=i+1) {
    L[i] = A[p + i];
  }
 
  L[i] = infinity;
  for (j = 0; j < n2; j=j+1) {
    R[j] = A[q + j + 1];
  }
  R[j] = infinity;

  i = 0;
  j = 0;

  for (k = p; k <= r; k=k+1) {
    if (L[i] <= R[j]) {
      A[k] = L[i];
      i=i+1;
    } else {
      A[k] = R[j];
      j=j+1;
    }
  }

  free(L);
  free(R);
}

void MergeSort(int *A, int lo, int hi) {
  if (lo == hi) {
    return;
  }
  int mid = (lo + hi) / 2;
  MergeSort(A, lo, mid);
  MergeSort(A, mid + 1, hi);
  Merge(A, lo, mid, hi);
}

int main() {
  int length = 0;
  int i = 0;
  scanf("%d", &length);
  int *A = malloc(sizeof(int) * length);
  for (i = 0; i < length; i=i+1) {
    scanf("%d", &A[i]);
  }
  MergeSort(A, 0, length - 1);
  for (i = 0; i < length; i=i+1) {
    printf("%d ", A[i]);
  }
  free(A);
}