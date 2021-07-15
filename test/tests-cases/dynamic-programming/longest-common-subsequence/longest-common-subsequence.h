int x_m_1_y_n_1 = 3;
int x_m_1_y_n = 2;
int x_m_y_n_1 = 1;

struct lcs_result {
  int **c;
  int **b;
  int length;
};

struct lcs_result LcsLength(char *x, char *y, int m, int n);
struct lcs_result LcsLengthMemoised(char *x, char *y, int m, int n);
void PrintLcs(int **b, char *x, char *y, int i, int j);