#include <vector>

#include "counting.h"

int choose(int n, int k)
{
  int i, c = 1;
  for (i = k + 1; i <= n; ++i) {
    c *= i;
    c /= i - k;
  }
  return c;
}

int findNumGridPoints(int d, int k)
{
  int out = 0;
  if(d > 0) {
    for(int i = 0; i < k + 1; ++i){
      out += choose(d - 1 + i, i) * (1 << i);
    }
  } else {
    out = 1;
  }
  return out;
}
