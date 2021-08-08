
int rec(int arg) {
  if (arg) {
    printf("non base\n");
    return rec(0);
  }
  else {
    printf("base\n");
    return 1;
  }
}

int main() {
  int x;
  x = rec(1);

  printf("%d", x);
}