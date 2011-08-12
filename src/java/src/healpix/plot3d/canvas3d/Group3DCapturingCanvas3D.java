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
package healpix.plot3d.canvas3d;

import java.io.File;
import javax.imageio.ImageIO;

import java.awt.GraphicsConfiguration;
import java.awt.image.BufferedImage;

import java.io.FileOutputStream;
import java.io.IOException;

import javax.media.j3d.Canvas3D;
import javax.media.j3d.GraphicsContext3D;
import javax.media.j3d.ImageComponent;
import javax.media.j3d.ImageComponent2D;
import javax.media.j3d.Raster;

import javax.vecmath.Point3f;

/**
 * Class CapturingCanvas3D, using the instructions from the Java3D FAQ pages on
 * how to capture a still image in jpeg format. If the writeJPEG flag is set to
 * true, the next update() writes a file, Group3DCaptureX.jpg where X is a
 * number count.
 * 
 * @author ejoliet
 * @version $Id: Group3DCapturingCanvas3D.java 26061 2007-06-27 09:47:54Z
 *          ulammers $
 */
public class Group3DCapturingCanvas3D extends Canvas3D {
	
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/**
	 * Flag that triggers the capture
	 * 
	 * @serial
	 */
	public boolean writeJPEG;

	/**
	 * Counter for filenames so that multiple canvases may be captured
	 * 
	 * @serial
	 */
	private int postSwapCount;

	/**
	 * Creates a new Group3DCapturingCanvas3D object.
	 * 
	 * @param gc
	 */
	public Group3DCapturingCanvas3D(GraphicsConfiguration gc) {
		super(gc);
		postSwapCount = 0;
	}

	/* (non-Javadoc)
	 * @see javax.media.j3d.Canvas3D#postSwap()
	 */
	public void postSwap() {
		if (writeJPEG) {
			System.out.println("Writing JPEG");

			GraphicsContext3D ctx = getGraphicsContext3D();

			// The raster components need all be set!
			Raster ras = new Raster(new Point3f(-1.0f, -1.0f, -1.0f),
					Raster.RASTER_COLOR, 0, 0, 512, 512, new ImageComponent2D(
							ImageComponent.FORMAT_RGB, new BufferedImage(512,
									512, BufferedImage.TYPE_INT_RGB)), null);

			ctx.readRaster(ras);

			// Now strip out the image info
			BufferedImage img = ras.getImage().getImage();

			// write that to disk....
			try {
                                ImageIO.write(img, "jpeg", new File("Group3DCapture"
						+ postSwapCount + ".jpg"));
			} catch (IOException e) {
				System.out.println("I/O exception: " + e.getMessage());
			}

			postSwapCount++;
		}
	}
}
