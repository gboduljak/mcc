struct s {
  int va;
  int vb;
};

struct s f(int a, int b) {
  struct s s; 
  s.va = a; 
  s.vb = b;
  return s;
} 

int main() { 
  struct s s2; 
  s2 = f(1,4); 
  printf("%d\n", f(5,2).vb);
}