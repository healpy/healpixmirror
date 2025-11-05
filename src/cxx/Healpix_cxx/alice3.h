#ifndef ALICE3_H
#define ALICE3_H

#include "healpix_map.h"

class PolarizationHolder
{
  public:
    Healpix_Map<double> Q, U;
    void getQU(const pointing &p, double &q, double &u) const;
    vec3 getQUDir(const vec3 &loc) const;
    double getQUMagnitude(const pointing& p) const;
};

void lic_main(const Healpix_Map<double> &Q, const Healpix_Map<double> &U, const Healpix_Map<double> &th,
  Healpix_Map<double> &hit, Healpix_Map<double> &tex, Healpix_Map<double> &mag,
  int steps, int kernel_steps, double step_radian, double polmin, double polmax);

#endif
