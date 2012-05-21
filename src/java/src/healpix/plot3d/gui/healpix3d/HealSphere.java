/*
 * HEALPix Java code supported by the Gaia project.
 * Copyright (C) 2006-2011 Gaia Data Processing and Analysis Consortium
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */
package healpix.plot3d.gui.healpix3d;

import healpix.essentials.HealpixBase;
import healpix.essentials.Scheme;
import healpix.essentials.Vec3;

import javax.media.j3d.Appearance;
import javax.media.j3d.ColoringAttributes;
import javax.media.j3d.Geometry;
import javax.media.j3d.LineArray;
import javax.media.j3d.Shape3D;
import javax.vecmath.Color3f;
import javax.vecmath.Point3d;

// ==============================================================================
/**
 * A base class for different healpix sphere representations (??). Adaptation by
 * R.Carr from earlier code by G.Giardino (& O'Mullane??).
 */
public abstract class HealSphere extends Shape3D
  {
  /** The nside. */
  protected int nside = 2;

  /**
    * Number of sections to use when drawing side of a pixel as series of
    * lines.
    */
  protected int step = 60;

  /** Descriptor object for a healpix sphere of a specific nside. */
  protected HealpixBase index;

  protected void setStep()
    {
    step=10;
    if (nside >= 8)
      step = 6;
    if (nside >= 16)
      step = 4;
    if (nside >= 32)
      step = 2;
    if (nside >= 64)
      step = 1;
    }

  /**
    * Constructor for specified nside.
    *
    * @param nside
    *            the healpix resolution parameter.
    */
  public HealSphere(int nside, Scheme scheme) {
    this.nside = nside;
    setStep();
    try {
            index = new HealpixBase(nside,scheme);
    } catch (Exception e) {
            e.printStackTrace();
    }
  }

  /**
    * Instantiates a new heal sphere.
    *
    * @param ind the ind
    */
  public HealSphere(HealpixBase ind) {
    if (ind != null) {
            this.index = ind;
    } else {
            try {
                    index = new HealpixBase(nside,Scheme.NESTED);
            } catch (Exception e) {
                    e.printStackTrace();
            }
    }
    this.nside = ind.getNside();
    setStep();
  }

  /**
    * Fetch the geometical objects describing the sphere, or whatever part of i
    * this object represents.
    *
    * @return the lines or polygons or whatever represent the geometry of this
    *         object.
    */
  protected abstract Geometry createGeometry();

  /**
    * Add lines corresponding to the outline of the pixel defined by the set of
    * points.
    *
    * @param points
    *            the vertices
    * @param offset
    *            the position in quads at which to start writing values.
    * @param quads
    *            structure into which to insert lines.
    */
  protected void addPix(Vec3[] points, int offset, LineArray quads) {
    int point = 0;
    for (int c = 0; c < points.length; c++) {
      // System.out.println("point:"+(offset+point)+" "+corners[c]);
      quads.setCoordinate((offset + point++), new Point3d(points[c].x,
                      points[c].y, points[c].z));
      if (c > 0) { // add it again
              quads.setCoordinate((offset + point++), new Point3d(points[c]
                              .x, points[c].y, points[c].z));
      }
    }
    quads.setCoordinate((offset + point++), new Point3d(points[0].x,
                    points[0].y, points[0].z));
  }

  /**
    * Convenience method for creating Appearance object.
    *
    * @return appearance object with color.
    */
  protected Appearance createAppearance() {

          Appearance app = new Appearance();
          ColoringAttributes ca = new ColoringAttributes(new Color3f(
                          java.awt.Color.GREEN), ColoringAttributes.SHADE_FLAT);
          app.setColoringAttributes(ca);

          return app;
  }
  }
