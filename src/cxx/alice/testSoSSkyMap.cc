#include <iostream>
#include "SoSSkyMap.h"
using namespace std;

int main()
  {
  SoSSkyMap s;
  s.set_size(4096);
  for (int i = 0; i <= s.max_pixel(); i++)
    {
    int x, y, j;
    s.i2xy(i, x, y);
    s.xy2i(x, y, j);
    assert(i == j);
    }

  for (int i = 0; i <= s.max_pixel(); i++)
    {
    double d = double(i);
    s.set_pixel(i, d);
    double d2 = s.get_pixel(i);
    assert(d == d2);
    }
  cout << "test passed a" << endl;

  for (int i = 0; i <= s.max_pixel(); i++)
    {
    pointing p = s.deproject(i);
    int j = s.project(p);
    assert(i == j);
    }
  cout << "test passed b" << endl;

  for (int i = 0; i < s.max_pixel(); i++)
    {
    int j = s.get_next_pixel(i);
    assert(j == i + 1);
    }
  cout << "test passed c" << endl;
  }
