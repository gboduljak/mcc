struct panj {
  int dimenzije[3];
  double cijenaPoKili;
};

// struct panj* opanji() {
//   struct panj* novi;
//   novi = (struct panj*) malloc(sizeof(struct panj));
//   novi->cijenaPoKili = 69.0;
//   novi->dimenzije[0] = 1;
//   novi->dimenzije[1] = 2;
//   novi->dimenzije[2] = 3;
//   return novi;
// }

void ispisi_panj(struct panj *p) {
  printf("%f \n", p->cijenaPoKili);
  printf("x : %d \n", p->dimenzije[0]);
  printf("y : %d \n", p->dimenzije[1]);
  printf("z : %d \n", p->dimenzije[2]);
}

// void primi_panj(struct panj p) {
//   printf("%f", p.cijenaPoKili);
// }

// step 1 -> rewrite struct panj to struct panj*
// step 2 -> add struct.result formal
/// .... 
/// struct panj* zanimljivo (struct panj* struct.result, int x, int y, int z) 
///step 3 -> when calling zanimljivo, alloca space for struct panj and pass to first arg
struct panj zanimljivo(int x, int y, int z) {
  struct panj p;
  p.dimenzije[0] = x;
  p.dimenzije[1] = y;
  p.dimenzije[2] = z;
  return p;
}

struct panj identitet(struct panj p) {
  return p;
}

int main() {
  struct panj prvi;
  struct panj* drugi;

  prvi = identitet(zanimljivo(1, 2, 3));

  ispisi_panj(&prvi);

  // drugi = opanji();

  // printf("%f \n", drugi->cijenaPoKili);
  // printf("x : %d \n", drugi->dimenzije[0]);
  // printf("y : %d \n", drugi->dimenzije[1]);
  // printf("z : %d \n", drugi->dimenzije[2]);

  // prvi.cijenaPoKili = 10.0;
  // ispisi_panj(&prvi);
  // primi_panj(prvi);
}