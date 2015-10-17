#include <iostream>
#include <vector>
#include <array>
#include <cstdlib>

using namespace std;
constexpr int coin_count = 8;

int coins[coin_count] = { 1, 10, 100, 500, 1000, 5000, 10000, 50000 };
array<vector<int>, coin_count + 1> dp;

int main(int argc, char **argv) {
  if (argc != 2) { return 1; }

  int value = atoi(argv[1]);
  for (auto& vec: dp) { vec.resize(value + 1); }

  for (int i = 0; i <= coin_count; ++i) { dp[i][0] = 1; }
  for (int j = 0; j <= value; ++j) { dp[0][j] = 0; }
  for (int i = 1; i <= coin_count; ++i) {
    for (int j = 1; j <= value; ++j) {
      dp[i][j] = j < coins[i - 1] ?
        dp[i - 1][j] : dp[i][j - coins[i - 1]] + dp[i - 1][j];
    }
  }

  cout << dp[coin_count][value] << endl;
  return 0;
}
