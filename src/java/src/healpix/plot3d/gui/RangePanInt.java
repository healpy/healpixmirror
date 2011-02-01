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
package healpix.plot3d.gui;

import java.awt.event.TextEvent;
import java.awt.event.TextListener;

/**
 * @author ejoliet
 * @version $Id: RangePanInt.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class RangePanInt extends RangePanel implements TextListener {
    
    /** The Constant serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Name and attrib will prbably be the same but may differ slightly
     */
    public RangePanInt(String name, String get, String attrib, String cls) {
        super(name, get, attrib, cls);
        critInit(get, attrib, cls);
    }

    /* (non-Javadoc)
     * @see healpix.plot3d.gui.RangePanel#critInit(java.lang.String, java.lang.String, java.lang.String)
     */
    protected void critInit(String get, String attrib, String cls) {
    }

    /* (non-Javadoc)
     * @see healpix.plot3d.gui.RangePanel#textValueChanged(java.awt.event.TextEvent)
     */
    public void textValueChanged(TextEvent e) {
    }
}
