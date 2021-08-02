int coins_n;
int amount;

int *coins;
int *dp;
int *coin_used;

int infinity;

int min(int a, int b) {
  if (a < b) {
    return a;
  }
  return b;
}

void optimally_exchange(int amount) {
  int exch_amt;
  int i;

  exch_amt = 0;
  i = 1;

  for (exch_amt = 1; exch_amt <= amount; exch_amt = exch_amt + 1) {
    dp[exch_amt] = infinity;
  }

  dp[0] = 0;

  for (exch_amt = 1; exch_amt <= amount; exch_amt = exch_amt + 1) {
    for (i = 0; i < coins_n; i = i + 1) {
      if (coins[i] <= exch_amt) {
        if (1 + dp[exch_amt - coins[i]] < dp[exch_amt]) {
          dp[exch_amt] = 1 + dp[exch_amt - coins[i]];
          coin_used[exch_amt] = coins[i];
        }
      }
    }
  }
}

void print_exchange(int n) {
  printf("%d", coin_used[n]);
  if (n - coin_used[n]) {
    printf(" + ");
    print_exchange(n - coin_used[n]);
  }
}

int main() {
  coins_n = 0;
  amount = 0;
  coins = (int *)NULL;
  dp = (int *)NULL;
  coin_used = (int *)NULL;
  infinity = 10000000;

  int i;
  i = 0;
  scanf("%d %d", &coins_n, &amount);

  coins = (int *)malloc(sizeof(int) * (coins_n));
  dp = (int *)malloc(sizeof(int) * (amount + 1));
  coin_used = (int *)malloc(sizeof(int) * (amount + 1));

  for (i = 0; i < coins_n; i = i + 1) {
    scanf("%d", &coins[i]);
  }
  for (i = 0; i < coins_n; i = i + 1) {
    printf("%d\n", coins[i]);
  }

  optimally_exchange(amount);

  printf("optimal exchange uses %d coins\n", dp[amount]);
  printf("optimal change is %d = ", amount);
  print_exchange(amount);

  free((void *)coins);
  free((void *)dp);
  free((void *)coin_used);
  return 0;
}