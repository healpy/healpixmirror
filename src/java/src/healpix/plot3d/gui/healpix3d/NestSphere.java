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
import healpix.essentials.Vec3;
import healpix.essentials.Scheme;

import javax.media.j3d.Geometry;
import javax.media.j3d.GeometryArray;
import javax.media.j3d.LineArray;

/**
 * The Class NestSphere.
 */
public class NestSphere extends HealSphere
  {
  /** The face. */
  protected int face = 0;

  /**
   * Instantiates a new nest sphere.
   *
   * @param nside the nside
   * @param face the face
   */
  public NestSphere(int nside,int face)
    {
    super(nside,Scheme.NESTED);
    this.face=face;
    this.setGeometry(createGeometry());
    this.setAppearance(createAppearance());
    }

  /*  Render the given face to given resolution */
  /* (non-Javadoc)
   * @see healpix.plot3d.gui.healpix3d.HealSphere#createGeometry()
   */
  protected Geometry createGeometry()
    {
    int nQuads = (int) (nside*nside);// one face
    int ppq =step*8; // points per quad
    int nPoints = nQuads* ppq ;
    int faceoff = nQuads*face;
    LineArray quads = new LineArray(nPoints, GeometryArray.COORDINATES);
    try
      {
      for (int q=0; q< nQuads; q++)
        {
        Vec3[] points = index.boundaries(faceoff+q,step);
        addPix(points,q*ppq,quads);
        }
      }
    catch (Exception e)
      {
      e.printStackTrace();
      }
    return quads;
    }
  }
