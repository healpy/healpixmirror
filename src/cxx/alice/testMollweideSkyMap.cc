#include <iostream>
#include <cmath>
#include "MollweideSkyMap.h"
#include "lsconstants.h"

using namespace std;

int main()
  {
  MollweideSkyMap m;
  m.set_size(2048);

  int i, j, x, y, x2, y2;
  double xp, yp;

  for(i = 0; i <= m.max_pixel(); i++)
    {
    m.i2xy(i, x, y);
    m.xy2xpyp(x, y, xp, yp);
    m.xpyp2xy(xp, yp, x2, y2);
    planck_assert(x2==x, "test a failed");
    planck_assert(y2==y, "test a failed");
    m.xy2i(x2, y2, j);
    planck_assert(i==j, "test a failed");
    }
  cout << "test passed a" << endl;

  for(i=0; i<=m.max_pixel(); i++)
    if (m.is_valid_pixel(i))
      {
      pointing p = m.deproject(i);
      j = m.project(p);
      planck_assert(i==j, "test b failed");
      }
  cout << "test passed b" << endl;
  }
