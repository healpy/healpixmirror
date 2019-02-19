/* gifstr: add a string to a GIF image, using GD library functions
   M. Bartelmann, Dec. 17, 1998 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gd.h"
#include "gdfontt.h"
#include "gdfonts.h"
#include "gdfontmb.h"
#include "gdfontl.h"
#include "gdfontg.h"

void gifstr  (
	      int *x, int *nx, int *ny,
	      int *sx, int *sy,
	      int *nc, int *r, int *g, int *b,
	      int *or, char *fn) {
  gdImagePtr im;
  gdFontPtr  ft;
  int        i,j,c;
  int        xs, ys;
  int        *ct;

  if (*nx < 300) {
    ft = gdFontTiny;
  } else if (*nx < 600) {
    ft = gdFontSmall;
  } else if (*nx < 900) {
    ft = gdFontMediumBold;
  } else if (*nx < 1200) {
    ft = gdFontLarge;
  } else {
    ft = gdFontGiant;
  }

  /* create a new image with appropriate dimensions */

  im = gdImageCreate(*nx, *ny);

  /* allocate color table */

  ct = (int *)malloc(*nc*sizeof(int));
  for (i = 0; i < *nc; i++) {
    ct[i] = gdImageColorAllocate(im, r[i], g[i], b[i]);
  }

  /* set pixel colors in image */

  for (i = 0; i < *nx; i++) {
    for (j = 0; j < *ny; j++) {
      c = x[i + j * *nx];
      gdImageSetPixel(im, i, j, ct[c]);
    }
  }

  ys = *sy - ft->h / 2;
  if (*or < 0) {
    xs = *sx - ((strlen(fn) + 1) * ft->w);
  } else if (*or > 0) {
    xs = *sx + ft->w;
  } else {
    xs = *sx - (strlen(fn) * ft->w / 2);
  }

  gdImageString(im, ft,	xs, ys,	fn, ct[0]);

  for (i = 0; i < *nx; i++) {
    for (j = 0; j < *ny; j++) {
      x[i + j * *nx] = gdImageGetPixel(im, i, j);
    }
  }

  /* destroy image */

  gdImageDestroy(im);
}
