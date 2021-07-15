int coins_n = 0;
int amount = 0;

int *coins = NULL;
int *dp = NULL;
int *coin_used= NULL;

int infinity = 10000000;

int min(int a, int b) { 
  if (a < b) {
    return a;
  }
  return b;
}

void optimally_exchange(int amount) {
  int exchange_amount = 0;
  int i = 1;

  for (exchange_amount = 1; exchange_amount <= amount; exchange_amount=exchange_amount+1) {
    dp[exchange_amount] = infinity;
  }

  dp[0] = 0;

  for (exchange_amount = 1; exchange_amount <= amount; exchange_amount=exchange_amount+1) {
    for (i = 0; i < coins_n; i=i+1) {
      if (coins[i] <= exchange_amount) {
        if (1 + dp[exchange_amount - coins[i]] < dp[exchange_amount]) {
          dp[exchange_amount] = 1 + dp[exchange_amount - coins[i]];
          coin_used[exchange_amount] = coins[i];
        }
      }
    }
  }
}

void print_exchange(int n) {
  printf("%d", coin_used[n]);
  if (n - coin_used[n]) {
    printf(" + "), print_exchange(n - coin_used[n]);
  }
}

int main() {
  int i = 0;
  scanf("%d %d", &coins_n, &amount);

  coins = (int *)malloc(sizeof(int) * (coins_n));
  dp = (int *)malloc(sizeof(int) * (amount + 1));
  coin_used = (int *)malloc(sizeof(int) * (amount + 1));

  for (i = 0; i < coins_n; i=i+1) { 
    scanf("%d", &coins[i]); 
  }
  for (i = 0; i < coins_n; i=i+1) {
    printf("%d\n", coins[i]);
  }

  optimally_exchange(amount);

  printf("optimal exchange uses %d coins\n", dp[amount]);
  printf("optimal change is %d = ", amount);
  print_exchange(amount);

  free(coins);
  free(dp);
  free(coin_used);
  return 0;
}