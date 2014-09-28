#ifndef HEALPIXSKYMAP
#define HEALPIXSKYMAP

#include "SkyMap.h"

class HealpixSkyMap: public SkyMap
  {
  private:
    Healpix_Map<float> map;
  public:
    HealpixSkyMap(int nside) : map(nside, RING,SET_NSIDE) { map.fill(0.); }
    virtual double get_pixel(int i) const {return map[i];}
    virtual void set_pixel(int i, double val) {map[i]=val;}
    virtual void add_to_pixel(int i, double val) {map[i]+=val;}
    virtual bool is_valid_pixel(int i) const { return ((i>=0)&&(i<map.Npix()));}
    virtual int max_pixel() const {return map.Npix();}
    virtual int get_next_pixel(int i) const {return i+1;}
    virtual int project(pointing p) const {return map.ang2pix(rotate(p)); }
    virtual pointing deproject(int i) const { return unrotate(map.pix2ang(i)); }
    virtual void minmax(float &min, float &max) const {map.minmax(min,max);}
    
    const Healpix_Map<float> &getMap() const { return map;}
  };

#endif // HEALPIXSKYMAP
