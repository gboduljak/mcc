struct moj_strukt {
  int x[10];
};


struct moj_strukt f(struct moj_strukt x) {
  return x;
} 

int main() {
  struct moj_strukt zajeb;
  zajeb.x[0] = 0;
  zajeb.x[1] = 1;
  zajeb.x[2] = 2;

  struct moj_strukt rezultat = f(zajeb);

  printf("%d\n", rezultat.x[0]);
}
