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

import healpix.plot3d.gui.DialogToolTip;
import healpix.plot3d.gui.healpix3d.HealpixPixelDataSphere;
import healpix.plot3d.gui.healpix3d.QuadArrayExt;

import java.awt.AWTEvent;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.Enumeration;

import javax.media.j3d.Bounds;
import javax.media.j3d.BranchGroup;
import javax.media.j3d.Canvas3D;
import javax.media.j3d.QuadArray;
import javax.media.j3d.Shape3D;
import javax.media.j3d.WakeupCondition;
import javax.media.j3d.WakeupCriterion;
import javax.media.j3d.WakeupOnAWTEvent;
import javax.media.j3d.WakeupOr;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ToolTipManager;
import javax.vecmath.Point3d;

import com.sun.j3d.utils.picking.PickCanvas;
import com.sun.j3d.utils.picking.PickIntersection;
import com.sun.j3d.utils.picking.PickResult;
import com.sun.j3d.utils.picking.behaviors.PickMouseBehavior;

/**
 * A behavior similar to SelectionBehavior, except you don't have to
 * double-click and the resulting info is presented in a tool-tip, not the
 * console.Using swings default (ToolTipManager) initialDelay constant.
 * 
 * @author ejoliet
 * @version $Id: ToolTipBehavior.java 120812 2010-01-24 23:12:12Z ejoliet $
 */
public class ToolTipBehavior extends PickMouseBehavior {
	
	/** The canvas. */
	Canvas3D canvas;

	/** The jt. */
	JTextArea jt = new JTextArea(50, 40);

	/** The jf. */
	JFrame jf = new JFrame();

	/**
	 * Constructor.
	 * 
	 * @param canvas
	 *            the canvas on which the behaviour is set
	 * @param root
	 *            the root element of the scene
	 * @param bounds
	 *            the bounds beahviour
	 */
	public ToolTipBehavior(Canvas3D canvas, BranchGroup root, Bounds bounds) {// ,GUIState
		super(canvas, root, bounds);
		jf.add(new JScrollPane(jt));
		jf.pack();
		this.canvas = canvas;
		this.setSchedulingBounds(bounds);
		root.addChild(this);
		setTolerance(0.1f);
		pickCanvas.setMode(PickCanvas.GEOMETRY_INTERSECT_INFO);// GEOMETRY_INTERSECT_INFO);

		timer = new javax.swing.Timer(ToolTipManager.sharedInstance()
				.getInitialDelay(), timeUpNoMovement);
		timer.setRepeats(false);
	}

	/** The time up no movement. */
	ActionListener timeUpNoMovement = new ActionListener() {
		public void actionPerformed(ActionEvent value0) {
			showing = true;
			updateScene(lastPoint.x, lastPoint.y);
			// I don't think I need to use an
			// invokeLater, the timer runs on
			// swing's thread (i think: "timer
			// uses the same thread used to make
			// cursors blink, tool tips appear,
			// and so on.")
			//
			// Yeap, it's "AWT-EventQueue-0".
		}
	};

	/** The my wakeup condition. */
	WakeupCondition myWakeupCondition;

	/** The timer. */
	javax.swing.Timer timer;

	/** The last point. */
	Point lastPoint = null;

	/**
	 * @see <code>http://archives.java.sun.com/cgi-bin/wa?A2=ind0201&L=java3d-interest&F=&S=&P=4228</code>
	 */
	public void initialize() {
		myWakeupCondition = new WakeupOr(new WakeupCriterion[] {
				new WakeupOnAWTEvent(MouseEvent.MOUSE_MOVED),
				new WakeupOnAWTEvent(MouseEvent.MOUSE_PRESSED),
				new WakeupOnAWTEvent(MouseEvent.MOUSE_ENTERED),
				new WakeupOnAWTEvent(MouseEvent.MOUSE_EXITED) });
		// the first two are standard (that's what wakeupCondition uses)
		// I also need mouse_exitsx [and maybe entered], so here we go.
		wakeupOn(myWakeupCondition);
	}

	/** The showing. */
	boolean showing = false;

	/** The can show. */
	boolean canShow = false;

	/**
	 * Set whether the tooltip is shown or not.
	 * @param val
	 */
	public void setCanShowToolTips(boolean val) {
		canShow = val;
		if (!canShow) {
			jf.setVisible(false);
			DialogToolTip.hideToolTip();
			timer.stop();
			showing = false;
		} else {
			jf.setVisible(false);
		}
	}

	/**
	 * Get if the tooltip behaviour is on or off
	 * @return tooltip behaviour state
	 */
	public boolean getCanShowToolTips() {
		return canShow;
	}

	/**
	 * Process the mouse events.
	 * 
	 * @param criteria
	 */
	public void processStimulus(Enumeration criteria) {
		WakeupCriterion wakeup;
		AWTEvent[] evt = null;

		while (criteria.hasMoreElements()) {
			wakeup = (WakeupCriterion) criteria.nextElement();
			if (wakeup instanceof WakeupOnAWTEvent)
				evt = ((WakeupOnAWTEvent) wakeup).getAWTEvent();

			// movement MouseEvets can be grouped together, I just want the last
			// one.
			AWTEvent levt = evt[evt.length - 1];

			if (levt instanceof MouseEvent) {
				MouseEvent mlevt = (MouseEvent) levt;
				int mlevtId = mlevt.getID();
				if (mlevtId == MouseEvent.MOUSE_EXITED) {
					DialogToolTip.hideToolTip();
					timer.stop();
					showing = false;
					jf.setVisible(false);
				} else if (canShow) {
					lastPoint = mlevt.getPoint();
					if (showing)
						updateScene(lastPoint.x, lastPoint.y);
					// maybe I should schedule this instead of calling it
					// directly.
					// This is the J3D thread, not the AWT/SWING thread. Got
					// sync?
					else
						timer.restart();// reset timer
				}
			}
		}
		wakeupOn(myWakeupCondition);
	}

	/** The Constant CURSOR_SKIP. */
	static final int CURSOR_SKIP = 20;

	/* (non-Javadoc)
	 * @see com.sun.j3d.utils.picking.behaviors.PickMouseBehavior#updateScene(int, int)
	 */
	public void updateScene(int xpos, int ypos) {
		PickResult pickResult = null;
		PickResult pickResults[] = null;
		QuadArrayExt pixel = null;
		Shape3D shape = null;
		pickCanvas.setShapeLocation(xpos, ypos);

		Point3d eyePos = pickCanvas.getStartPosition();

		if (!pickCanvas.getBranchGroup().isLive())
			return;
		// it's too soon for tooltips, the model is not ready yet.

		pickResults = pickCanvas.pickAll();
		// pickCanvas.getLocale().
		if (pickResults == null)
			return;

		// keep all picks to remove duplicates
		// LocationWrapper[] picks = new LocationWrapper[pickResults.length];
		/**
		 * Keep this for debugging!
		 * 
		 * String log = ""; for (int i = 0; i < pickResults.length; i++) { log +=
		 * (i + " -. " + pickResults[i].toString()) + "\n"; } jt.setText(log);
		 * log = ""; jf.setTitle("Pickresult:" + pickResults.length);
		 */
		String htmlText = null;
		for (int i = 0; i < pickResults.length; i++) {
			pickResult = pickResults[i];
			shape = (Shape3D) pickResult.getNode(PickResult.SHAPE3D);
			if (pickResult.getGeometryArray() instanceof QuadArrayExt) {
				pixel = (QuadArrayExt) pickResult.getGeometryArray();
			}
			if (shape instanceof HealpixPixelDataSphere) {
				pixel = (QuadArrayExt) pickResult.getGeometryArray();
				htmlText = pixel.getToolTipTxt();
			}
			PickIntersection pickIntersection = pickResult
					.getClosestIntersection(eyePos);
			if (pickIntersection != null) {
				final int[] indices;
				if (pickIntersection.getGeometryArray() instanceof QuadArray) {
					indices = pickIntersection.getPrimitiveVertexIndices();
					if (pixel != null) {// Not a 2D text!
						htmlText = pixel.getToolTipTxt(indices[2]); // any of
						// the
						// indices[0,...,3]
						// will give you
						// the right value
						// cause is a
						// quad!
						
						//updateColor(pixel, indices);
						break;
					}
				}
			}
			if (pickResult.numGeometryArrays() > 0) {
				PickIntersection pi = pickResult.getClosestIntersection(eyePos);

				// ... has intersections -- for reasons we cannot explain,
				// the pick intersection may be null, but the shape may NOT be
				// null!
				// mystifying....
				if (pi != null) {
					if (htmlText == null) {
						htmlText = "no data here!";// fPortrayal.getName(filledLW);
					}
				}
			}
		}
		Point s = canvas.getLocationOnScreen();
		s.x += lastPoint.x;
		s.y += lastPoint.y + CURSOR_SKIP;

		// if (htmlText != null)
		// htmlText = "null";//"<html><font size=\"-1\" face=\""+
		// sim.util.WordWrap.toHTML(canvas.getFont().getFamily())+ "\">" +
		// htmlText + "</font></html>";
		DialogToolTip.showToolTip(s, htmlText);
	}

}
