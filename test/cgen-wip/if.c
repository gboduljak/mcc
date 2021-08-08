int main() {
  int x;
  int y;
  int* ptr;
  x = 1;
  y = 2;
  printf("%d", x + y);
  if (x + y <= 1 + 2) {
    printf("meme\n");
  } else {
    printf("tech lead\n");
  }
  double z;
  z = 12.0;

  int w;
  w = 0;
  printf("%d", !x);
  if (!w) {
    printf("not 0 is indeed 1\n");
  }
  if (!x == 0) {
    printf("not 1 is indeed 0\n");
  }

  if (- (x - y) > 0) {
    printf("negative is indeed negative\n");
  }
}