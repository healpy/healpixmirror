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
package healpix.plot3d.gui.view;

import java.awt.GridLayout;

import javax.swing.JApplet;
import javax.swing.JTextArea;

/**
 * This class is the applet used to launch the Map Viewer from a web page.
 * It checks that the user has the right Java version installed and then
 * instantiates the MapView3d Frame, this however doesn't work as any VM prior to 1.5
 * will complain about the class version being 49.0
 * 
 * @version $Id: MapView3dApplet.java 49444 2008-05-07 10:23:02Z ejoliet $
 * @author mtlinden
 */
public class MapView3dApplet extends JApplet {
	/**
	 * Default UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * TextArea used as a console to show messages.
	 */
	private final JTextArea textArea = new JTextArea();

	/** The frame. */
	private MapView3d frame;

	/**
	 * Initialize the applet. Instantiate the MapView3d frame in its own thread.
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
				MapView3d frame = new MapView3d(true);
				setFrame(frame);
				frame.setVisible(true);
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
	 * Sets the frame.
	 * 
	 * @param frame the new frame
	 */
	protected void setFrame(MapView3d frame) {
		this.frame = frame;
	}

	/**
	 * Gets the frame.
	 * 
	 * @return the frame
	 */
	public MapView3d getFrame() {
		return this.frame;
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
