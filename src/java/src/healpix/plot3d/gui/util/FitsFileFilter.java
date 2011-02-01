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
package healpix.plot3d.gui.util;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/**
 * Filter to select FITS files.
 *
 * @author ejoliet
 * @version $Id: FitsFileFilter.java 26046 2007-06-27 08:38:02Z mterlind $
 */
public class FitsFileFilter extends FileFilter {

    /* (non-Javadoc)
     * @see javax.swing.filechooser.FileFilter#accept(java.io.File)
     */
    @Override
    public boolean accept(File f) {
        String extension = getExtension(f);
        if ((extension.equals("ds")) || (extension.equals("fits"))) {
            return true;
        }
        return false;
    }

    /* (non-Javadoc)
     * @see javax.swing.filechooser.FileFilter#getDescription()
     */
    @Override
    public String getDescription() {
        return "FITS file";
    }

    /**
     * Get the file extension in lower case
     * 
     * @param f
     * @return the file extension
     */
    private String getExtension(File f) {
        String s = f.getName();
        int i = s.lastIndexOf('.');
        if (i > 0 &&  i < s.length() - 1) {
            return s.substring(i+1).toLowerCase();
        }
        return "";
    }
}