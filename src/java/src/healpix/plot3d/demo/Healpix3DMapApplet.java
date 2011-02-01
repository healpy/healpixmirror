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
package healpix.plot3d.demo;

import healpix.core.dm.HealpixMap;
import healpix.tools.HealpixMapCreator;

import java.awt.GridLayout;

import javax.swing.JApplet;
import javax.swing.JTextArea;

/**
 * This class is the applet used to launch Healpix3DMap from a web page.
 * It checks that the user has the right Java version installed and then
 * instantiates the Healpix3DMap, this however doesn't work as any VM prior to 1.5
 * will complain about the class version being 49.0
 * 
 * @version $Id: Healpix3DMapApplet.java 26131 2007-06-27 16:02:03Z ejoliet $
 * @author mtlinden
 */
public class Healpix3DMapApplet extends JApplet {
	/**
	 * Default UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * TextArea used as a console to show messages.
	 */
	private final JTextArea textArea = new JTextArea();

	/**
	 * Initialize the applet. Instantiate the MdtFrame in its own thread.
	 */
	public void init() {
		textArea.setEditable(false);
		getContentPane().setLayout(new GridLayout(1, 0));
		getContentPane().add(textArea);
		addLine("Please wait while starting applet...");

		// simple Java version check
		String v = System.getProperty("java.version");
		v = v.substring(0, 5);
		if (v.compareTo("1.5.0") < 0) {
			addLine("ERROR - Java version not correct, at least version 1.5.0 required.");
			return;
		}

        // starts application frame and wait until finished
		Runnable frameThread = new Runnable() {
			public void run() {
				Healpix3DMap frame = new Healpix3DMap();
				HealpixMapCreator cr = new HealpixMapCreator();
				HealpixMap map = cr.getMap();
				System.out.println("********** Map nside=" + map.nside());
				System.out.println("********** Map pixels=" + map.nPixel());
				frame.setMap(map);
				while (!frame.isDone()) {
					try {
						Thread.sleep(1000);
					} catch (Exception e) {
					}
				}
				addLine("Finished.");
			}
		};
		new Thread(frameThread).start();
	}

	/**
	 * Shows a message in the console.
	 * 
	 * @param line
	 *            the message to be shown.
	 */
	private void addLine(String line) {
		textArea.append(line + "\n");
		repaint();
	}
}
