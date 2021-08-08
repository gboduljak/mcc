struct structito {
  int arr[10][20][30];
};


int main() {
  struct structito s;
  s.arr[1][2][3] = 1;
  printf("%d", s.arr[1][2][3]);
}