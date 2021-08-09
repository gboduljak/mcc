
struct dependent_struct
{
  struct dependent_struct inner;
  struct dependent_struct outer;

  /* data */
};

struct my_struct {
  int x;
  double y;
  char z;
  struct dependent_struct dep;
};


int main (){
  
  struct my_struct x;


  // struct st y;
  // y.x = 1;
  // y.y = 2;
  // y.z = 3;

  // y;
  // printf("%d", y.x);
}