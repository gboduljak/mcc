
int* row(int** mat, int row, int n) {
  int* rowPtr;
  int col;
  rowPtr = (int*)malloc(sizeof(int) * n);

  for (col = 0; col < n; col = col + 1) {
    rowPtr[col] = mat[row][col];
  }

  return rowPtr;
}


int* col(int** mat, int col, int n) {
  int* colPtr;
  int row;
  colPtr = (int*)malloc(sizeof(int) * n);

  for (row = 0; row < n; row = row + 1) {
    colPtr[row] = mat[row][col];
  }

  return colPtr;
}


int dot(int* vec1, int* vec2, int n) {
  int dot;
  int i;
  dot = 0;

  for (i = 0; i < n; i=i+1) {
    dot = dot + (*(vec1 + i)) * (*(vec2 + i));
  }

  return dot;
}

int **matmul(int **left, int ** right, int n) {
  int** prod;
  int i;
  int j;
  prod = (int**)malloc(sizeof(int*) * n);

  for (i = 0; i < n; i = i + 1) {
    prod[i] = (int*)malloc(sizeof(int) * n);
  }

  for (i = 0; i < n; i = i + 1) {
    for (j = 0; j < n; j = j + 1) {
      int* rowVec;
      int* colVec;

      rowVec = row(left, i, n);
      colVec = col(right, j, n);
      prod[i][j] = dot(rowVec, colVec, n);

      free((void*)rowVec);
      free((void*)colVec); 
    }
  }

  return prod;
}

void printMat(int** mat, int n) {
  int i;
  int j;

  for (i = 0; i < n; i = i + 1) {
    for (j = 0; j < n; j = j + 1) {
      printf("%d ", mat[i][j]);
    }
    printf("\n");
  }
}

int main() {

  int n;
  int** ptr;
  int** ptr2;
  int** ptrProd;
  int i;
  int j;
  int k;

  scanf("%d", &n);
  printf("matrix size is: %d \n", n);
  ptr = (int**)malloc(sizeof(int*) * n);
  ptr2 = (int**)malloc(sizeof(int*) * n);

  for (i = 0; i < n; i = i+1) {
    ptr[i] = (int*)malloc(sizeof(int) * n);
    ptr2[i] = (int*)malloc(sizeof(int) * n);
  }
  
  k = 0;

  for (i = 0; i < n; i = i + 1) {
    for (j = 0; j < n; j = j + 1) {
      ptr[i][j] = k;
      ptr2[i][j] = k;
      k = k + 1;
    }
  }

  printf ("mats are: \n");
  printf ("mat1: \n");
  printMat(ptr, n);
  printf ("mat2: \n");
  printMat(ptr2, n);

  int *meme_row;
  int* meme_col;
  meme_row = row(ptr, 0, n);
  meme_col = col(ptr, 0, n);
  for (i = 0; i < n; i = i + 1) {
    printf("%d\n", meme_row[i]);
  }
  ptrProd = matmul(ptr, ptr2, n);

  printf("matrix prod is: \n");

  printMat(ptrProd, n);
}