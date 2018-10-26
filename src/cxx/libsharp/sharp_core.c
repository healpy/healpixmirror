/*
 *  This file is part of libsharp.
 *
 *  libsharp is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  libsharp is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with libsharp; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

/*
 *  libsharp is being developed at the Max-Planck-Institut fuer Astrophysik
 *  and financially supported by the Deutsches Zentrum fuer Luft- und Raumfahrt
 *  (DLR).
 */

/*! \file sharp_core.c
 *  Computational core
 *
 *  Copyright (C) 2012-2013 Max-Planck-Society
 *  \author Martin Reinecke
 */

#define ARCH _default
#include "sharp_core_inc0.c"
#undef ARCH

#if (!defined(__AVX__)) && defined(__GNUC__) && defined (__x86_64__) && (__GNUC__>=5)
void inner_loop_avx (sharp_job *job, const int *ispair,const double *cth,
  const double *sth, int llim, int ulim, sharp_Ylmgen_C *gen, int mi,
  const int *mlim);
#endif

void inner_loop (sharp_job *job, const int *ispair,const double *cth,
  const double *sth, int llim, int ulim, sharp_Ylmgen_C *gen, int mi,
  const int *mlim)
  {
#if (!defined(__AVX__)) && defined(__GNUC__) && defined (__x86_64__) && (__GNUC__>=5)
  __builtin_cpu_init();
  if (__builtin_cpu_supports("avx"))
    inner_loop_avx (job, ispair, cth, sth, llim, ulim, gen, mi, mlim);
  else
#endif
    inner_loop_default (job, ispair, cth, sth, llim, ulim, gen, mi, mlim);
  }

int sharp_veclen(void)
  {
#if (!defined(__AVX__)) && defined(__GNUC__) && defined (__x86_64__) && (__GNUC__>=5)
  __builtin_cpu_init();
  if (__builtin_cpu_supports("avx"))
    return 4;
  else
#endif
    return VLEN;
  }
