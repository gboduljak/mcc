struct st {
  int x;
  int y;
  int z;
};

int main (){
  struct st y;
  y.x = 1;
  y.y = 2;
  y.z = 3;

  y;
  printf("%d", y.x);
}