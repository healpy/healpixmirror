// ALICE = Amazing Line Integral Convolution Executable
// Programmer: David Larson
// Date: January 29, 2006
// (Code originally written last fall.)
//
// Using default emacs indentation.


#include <iostream>
#include "paramfile.h"
#include "healpix_map_fitsio.h"
#include "lsconstants.h"
#include "arr.h"
#include "fitshandle.h"
#include "PolarizationHolder.h"
#include "TextureHolder.h"
#include "HealpixSkyMap.h"
#include "alice_utils.h"
#include "vec3.h"
#include "string_utils.h"

using namespace std;

int lic_function(SkyMap &hitcount, SkyMap &texture,
  const PolarizationHolder &ph, const TextureHolder &th, int steps,
  int kernel_steps, double step_radian)
  {
  arr< double > kernel(kernel_steps), convolution, rawtexture;
  make_kernel(kernel);
  arr< pointing > curve(steps);

  // Reset the texture to zero.
  for (int i=0; i<texture.max_pixel(); i++)
    texture.set_pixel(i, 0.0);

  int num_curves=0, k;

  for(int i=0; i<=texture.max_pixel(); i++)
    {
    if (texture.is_valid_pixel(i))
      {
      pointing p=texture.deproject(i);
      if (hitcount.get_pixel(i)<1.0)
        {
        num_curves++;
        runge_kutta_2(p.to_vec3(), ph, step_radian, curve);
        pointings_to_textures(curve, th, rawtexture);
        convolve(kernel, rawtexture, convolution);
        for (tsize j=0; j<convolution.size(); j++)
          {
          p = curve[j + kernel.size()/2];
          k = texture.project(p);
          if (texture.is_valid_pixel(k))
            {
            texture.add_to_pixel(k, convolution[j]);
            hitcount.add_to_pixel(k, 1.0);
            }
          }
        }
      }
    }
  return num_curves;
  }

// ----------------------------------------------------------------------
int main(int argc, const char** argv)
  {
  module_startup ("alice3", argc, argv);
  paramfile params (getParamsFromCmdline(argc,argv));

  PolarizationHolder ph;
  ph.load(params.find<string>("in"));

  int nside = params.find<int>("nside");
  int steps = params.find<int>("steps",100);
  double step_radian=10.*arcmin2rad;
  int kernel_steps = params.find<int>("kernel_steps",50);
  float polmin = params.find<float>("polmin",123456.0),
        polmax = params.find<float>("polmax",123456.0);
  string out = params.find<string>("out");

  // Texture
  TextureHolder th;
  if (params.param_present("texture"))
    th.load(params.find<string>("texture"));
  else
    {
    if (params.param_present("ell"))
      th.setToEllNoise(nside, params.find<int>("ell"));
    else
      th.setToWhiteNoise(nside);
    }

  HealpixSkyMap *hitcount = new HealpixSkyMap(nside),
         *texture = new HealpixSkyMap(nside),
         *magnitude = new HealpixSkyMap(nside);

  for (int i=0; i<magnitude->max_pixel(); i++)
    if (magnitude->is_valid_pixel(i))
      {
      pointing p = magnitude->deproject(i);
      p.normalize();

      float temp = ph.getQUMagnitude(p);
      if (polmax != 123456.0) temp = (temp > polmax) ? polmax : temp;
      if (polmin != 123456.0) temp = (temp < polmin) ? polmin : temp;
      magnitude->set_pixel(i, temp);

      texture->set_pixel(i, th.getTexture(p));
      }

  write_Healpix_map_to_fits(out+"_background.fits",texture->getMap(),
    PLANCK_FLOAT32);

  int num_curves = lic_function(*hitcount, *texture, ph, th,
    steps, kernel_steps, step_radian);

  Healpix_Map<float> tex(texture->getMap()),
                     mag(magnitude->getMap()),
                     hit(hitcount->getMap());
                     
  for (tsize i=0; i<tex.Npix(); ++i)
    tex[i]/=hit[i];
  float tmin,tmax,mmin,mmax;
  tex.minmax(tmin,tmax);
  mag.minmax(mmin,mmax);
  for (tsize i=0; i<tex.Npix(); ++i)
    {
    mag[i]=(tex[i]-tmin)*(mag[i]-mmin);
    tex[i]=1.0-0.95*(tex[i]-tmin)/(tmax-tmin);
    }
  mag.minmax(mmin,mmax);
  for (tsize i=0; i<mag.Npix(); ++i)
    mag[i]=1.0-0.95*(mag[i]-mmin)/(mmax-mmin);
  write_Healpix_map_to_fits(out+"_texture.fits",tex,PLANCK_FLOAT32);
  write_Healpix_map_to_fits(out+"_mod_texture.fits",mag,PLANCK_FLOAT32);
  int num_pix = 0;
  float hitmin, hitmax;

  hitcount->minmax(hitmin, hitmax);
  for (int i=0; i<=hitcount->max_pixel(); i++)
    if (hitcount->is_valid_pixel(i)) num_pix++;
  cout << endl;
  cout << "number of curves calculated = " << num_curves << endl;
  float foo = float(num_pix) / num_curves;
  cout << "number of pixels in image = " << num_pix << endl;
  cout << "average pixels per curve = " << foo << endl;
  cout << "average curve points per good pixel = " << steps / foo << endl;
  cout << "minimum number of hits = " << hitmin << endl;
  cout << "maximum number of hits = " << hitmax << endl;
  }

