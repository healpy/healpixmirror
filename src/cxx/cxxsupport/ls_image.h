/*
 *  This file is part of libcxxsupport.
 *
 *  libcxxsupport is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  libcxxsupport is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with libcxxsupport; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

/*
 *  libcxxsupport is being developed at the Max-Planck-Institut fuer Astrophysik
 *  and financially supported by the Deutsches Zentrum fuer Luft- und Raumfahrt
 *  (DLR).
 */

/*! \file ls_image.h
 *  Classes for creation and output of image files
 *
 *  Copyright (C) 2003-2015 Max-Planck-Society
 *  \author Martin Reinecke, David Larson
 */

#ifndef PLANCK_LS_IMAGE_H
#define PLANCK_LS_IMAGE_H

#include <string>
#include <vector>
#include <iostream>
#include <algorithm>
#include "arr.h"
#include "datatypes.h"
#include "colour.h"
#include "linear_map.h"

/*! \defgroup imagegroup Image creation */
/*! \{ */

/*! A class for mapping floting-point values into colours. */
class Palette: public linearMap<Colour>
  {
  public:
    /*! Adds a new data point \a f, with the corresponding colour \a c.
        The additions must be done in the order of ascending \a f. */
    void add (float f, const Colour &c)
      { addVal (f, c); }
    void addb (uint8 f, uint8 r,uint8 g, uint8 b)
      { addVal (f, Colour(r/255.,g/255.,b/255.)); }
    /*! Sets the palette to the predefined palette \a num. */
    void setPredefined(int num);
    /*! Returns the colour corresponding to the value \a f. The colour is
        determined by linear interpolation between neighbouring data points. */
    Colour Get_Colour (float f) const
      { return getVal_const(f); }
  };

class Colour8
  {
  private:
    void import (const Colour &col)
      {
      using namespace std;
      r = uint8(max(0,min(255,int(col.r*256))));
      g = uint8(max(0,min(255,int(col.g*256))));
      b = uint8(max(0,min(255,int(col.b*256))));
      }

  public:
    uint8 r,g,b;

    Colour8() {}
    Colour8 (uint8 R, uint8 G, uint8 B)
      : r(R), g(G), b(B) {}
    Colour8 (const Colour &col)
      { import (col); }
    const Colour8 &operator= (const Colour &col)
      { import (col); return *this; }
    bool operator== (const Colour8 &that)
      { return (r == that.r) && (g == that.g) && (b == that.b); }
    bool operator!= (const Colour8 &that)
      { return (r != that.r) || (g != that.g) || (b != that.b); }
  };

class MP_Font
  {
  public:
    int offset, num_chars, xpix, ypix;
    const char *data;
  };

extern const MP_Font medium_bold_font;
extern const MP_Font giant_font;

/*! Class for creating and storing image files. */
class LS_Image
  {
  private:
    MP_Font font;
    arr2<Colour8> pixel;

    void write_char (int xpos, int ypos, const Colour &col, char c,
                     int scale=1);

  public:
    /*! */
    LS_Image ();
    /*! Creates an image object with a resolution of \a xres by \a yres. */
    LS_Image (int xres, int yres);
    /*! */
    ~LS_Image () {}

    /*! Fills the entire image with colour \a col. */
    void fill (const Colour &col) { pixel.fill(col); }
    /*! Sets the font used for annotations to \a fnt. */
    void set_font (const MP_Font &fnt);
    /*! Outputs the string \a text in colour \a col.
        \a xpos, \a ypos is the lower left corner;
        the font is scaled by \a scale. */
    void annotate (int xpos, int ypos, const Colour &col,
      const std::string &text, int scale=1);
    /*! Outputs the string \a text centered at position \a xpos, \a ypos
        in colour \a col. The font is scaled by \a scale. */
    void annotate_centered (int xpos, int ypos, const Colour &col,
      const std::string &text, int scale=1);
    /*! Sets the pixel \a i, \a j, to the colour \a col. */
    void put_pixel (tsize i, tsize j, const Colour &col)
      {
      if ((i<pixel.size1()) && (j<pixel.size2()))
        pixel[i][j] = col;
      }
    /*! Returns the colour of the pixel \a i, \a j, or black if the pixel
        lies outside of the image. */
    Colour8 get_pixel (tsize i, tsize j)
      {
      return ((i<pixel.size1()) && (j<pixel.size2())) ?
        pixel[i][j] : Colour8(0, 0, 0);
      }

    /*! Writes the image to \a file in uncompressed TGA format. */
    void write_TGA (const std::string &file) const;

    /*! Writes the image to \a file in run-length encoded TGA format. */
    void write_TGA_rle (const std::string &file) const;

    /*! Writes the image to \a file in binary PPM format. */
    void write_PPM (const std::string &file) const;
  };

/*! \} */

#endif
