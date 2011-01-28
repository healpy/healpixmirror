#include <iostream>
#include <cmath>
#include "OrthogonalSkyMap.h"
#include "lsconstants.h"

using namespace std;

int main()
  {
  OrthogonalSkyMap orth(1024);

  int i, j, x, y, x2, y2;
  double xp, yp;

  for(i=0; i<=orth.max_pixel(); i++)
    {
    orth.i2xy(i, x, y);
    orth.xy2xpyp(x, y, xp, yp);
    orth.xpyp2xy(xp, yp, x2, y2);
    planck_assert(x2==x,"test a failed");
    planck_assert(y2==y,"test a failed");
    orth.xy2i(x2, y2, j);
    planck_assert(i==j,"test a failed");
    }
  cout << "test passed a" << endl;

  for(i = 0; i <= orth.max_pixel(); i++)
    if (orth.is_valid_pixel(i))
      {
      pointing p = orth.deproject(i);
      j = orth.project(p);
      planck_assert(i==j,"test b failed");
      }
  cout << "test passed b" << endl;
  }
