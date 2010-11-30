static void X(hp_map2alm) (int nside, int lmax, int mmax, FLT *xmap,
  FLT *xalm, double *zbounds, double *wgt)
  {
  psht_geom_info *ginfo;
  psht_alm_info *ainfo;
  X(joblist) *joblist;
  X(cmplx) *alm = (X(cmplx) *)xalm;
  FLT *map = xmap;

  CHECK_STACK_ALIGN(8);
  psht_make_healpix_geom_info_2 (nside, wgt, zbounds[0], zbounds[1], &ginfo);
  psht_make_rectangular_alm_info (lmax,mmax,1,&ainfo);

  X(make_joblist) (&joblist);
  X(add_job_map2alm) (joblist,map,alm,0);
  X(execute_jobs) (joblist, ginfo, ainfo);
  X(destroy_joblist) (joblist);

  psht_destroy_alm_info(ainfo);
  psht_destroy_geom_info(ginfo);
  }

static void X(hp_map2alm_pol) (int nside, int lmax, int mmax, FLT *xmap,
  FLT *xalm, double *zbounds, double *wgt)
  {
  psht_geom_info *ginfo;
  psht_alm_info *ainfo;
  long npix = 12*nside*nside;
  X(joblist) *joblist;
  X(cmplx) *alm = (X(cmplx) *)xalm;
  FLT *map = xmap;

  CHECK_STACK_ALIGN(8);
  psht_make_healpix_geom_info_2 (nside, wgt, zbounds[0], zbounds[1], &ginfo);
  psht_make_rectangular_alm_info (lmax,mmax,3,&ainfo);

  X(make_joblist) (&joblist);
  X(add_job_map2alm_pol) (joblist,map,map+npix,map+2*npix,alm,alm+1,alm+2,0);
  X(execute_jobs) (joblist, ginfo, ainfo);
  X(destroy_joblist) (joblist);

  psht_destroy_alm_info(ainfo);
  psht_destroy_geom_info(ginfo);
  }

static void X(hp_map2alm_spin) (int nside, int lmax, int mmax, int spin,
  FLT *xmap, FLT *xalm, double *zbounds, double *wgt)
  {
  psht_geom_info *ginfo;
  psht_alm_info *ainfo;
  long npix = 12*nside*nside;
  X(joblist) *joblist;
  X(cmplx) *alm = (X(cmplx) *)xalm;
  FLT *map = xmap;

  CHECK_STACK_ALIGN(8);
  psht_make_healpix_geom_info_2 (nside, wgt, zbounds[0], zbounds[1], &ginfo);
  psht_make_rectangular_alm_info (lmax,mmax,2,&ainfo);

  X(make_joblist) (&joblist);
  X(add_job_map2alm_spin) (joblist,map,map+npix,alm,alm+1,spin,0);
  X(execute_jobs) (joblist, ginfo, ainfo);
  X(destroy_joblist) (joblist);

  psht_destroy_alm_info(ainfo);
  psht_destroy_geom_info(ginfo);
  }

static void X(hp_alm2map) (int nside, int lmax, int mmax, FLT *xalm,
  FLT *xmap)
  {
  psht_geom_info *ginfo;
  psht_alm_info *ainfo;
  X(joblist) *joblist;
  X(cmplx) *alm = (X(cmplx) *)xalm;
  FLT *map = xmap;

  CHECK_STACK_ALIGN(8);
  psht_make_healpix_geom_info (nside, 1, &ginfo);
  psht_make_rectangular_alm_info (lmax,mmax,1,&ainfo);

  X(make_joblist) (&joblist);
  X(add_job_alm2map) (joblist,alm,map,0);
  X(execute_jobs) (joblist, ginfo, ainfo);
  X(destroy_joblist) (joblist);

  psht_destroy_alm_info(ainfo);
  psht_destroy_geom_info(ginfo);
  }

static void X(hp_alm2map_pol) (int nside, int lmax, int mmax, FLT *xalm,
  FLT *xmap)
  {
  psht_geom_info *ginfo;
  psht_alm_info *ainfo;
  long npix = 12*nside*nside;
  X(joblist) *joblist;
  X(cmplx) *alm = (X(cmplx) *)xalm;
  FLT *map = xmap;

  CHECK_STACK_ALIGN(8);
  psht_make_healpix_geom_info (nside, 1, &ginfo);
  psht_make_rectangular_alm_info (lmax,mmax,3,&ainfo);

  X(make_joblist) (&joblist);
  X(add_job_alm2map_pol) (joblist,alm,alm+1,alm+2,map,map+npix,map+2*npix,0);
  X(execute_jobs) (joblist, ginfo, ainfo);
  X(destroy_joblist) (joblist);

  psht_destroy_alm_info(ainfo);
  psht_destroy_geom_info(ginfo);
  }

static void X(hp_alm2map_spin) (int nside, int lmax, int mmax, int spin,
  FLT *xalm, FLT *xmap)
  {
  psht_geom_info *ginfo;
  psht_alm_info *ainfo;
  long npix = 12*nside*nside;
  X(joblist) *joblist;
  X(cmplx) *alm = (X(cmplx) *)xalm;
  FLT *map = xmap;

  CHECK_STACK_ALIGN(8);
  psht_make_healpix_geom_info (nside, 1, &ginfo);
  psht_make_rectangular_alm_info (lmax,mmax,2,&ainfo);

  X(make_joblist) (&joblist);
  X(add_job_alm2map_spin) (joblist,alm,alm+1,map,map+npix,spin,0);
  X(execute_jobs) (joblist, ginfo, ainfo);
  X(destroy_joblist) (joblist);

  psht_destroy_alm_info(ainfo);
  psht_destroy_geom_info(ginfo);
  }

FCALLSCSUB7(X(hp_map2alm),Y(PSHT_HP_MAP2ALM_X),Z(psht_hp_map2alm_x),INT,INT,INT,PFLT,PFLT,PDOUBLE,PDOUBLE)
FCALLSCSUB7(X(hp_map2alm_pol),Y(PSHT_HP_MAP2ALM_POL_X),Z(psht_hp_map2alm_pol_x),INT,INT,INT,PFLT,PFLT,PDOUBLE,PDOUBLE)
FCALLSCSUB8(X(hp_map2alm_spin),Y(PSHT_HP_MAP2ALM_SPIN_X),Z(psht_hp_map2alm_spin_x),INT,INT,INT,INT,PFLT,PFLT,PDOUBLE,PDOUBLE)
FCALLSCSUB5(X(hp_alm2map),Y(PSHT_HP_ALM2MAP_X),Z(psht_hp_alm2map_x),INT,INT,INT,PFLT,PFLT)
FCALLSCSUB5(X(hp_alm2map_pol),Y(PSHT_HP_ALM2MAP_POL_X),Z(psht_hp_alm2map_pol_x),INT,INT,INT,PFLT,PFLT)
FCALLSCSUB6(X(hp_alm2map_spin),Y(PSHT_HP_ALM2MAP_SPIN_X),Z(psht_hp_alm2map_spin_x),INT,INT,INT,INT,PFLT,PFLT)
