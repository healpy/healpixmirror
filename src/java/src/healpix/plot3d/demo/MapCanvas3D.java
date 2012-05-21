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

import healpix.essentials.HealpixBase;
import healpix.essentials.Scheme;
import healpix.core.dm.HealpixMap;
import healpix.plot3d.canvas3d.Group3DAxis;
import healpix.plot3d.canvas3d.Group3DCircle;
import healpix.plot3d.canvas3d.Group3DSphere;
import healpix.plot3d.gui.healpix3d.DataSphere;
import healpix.plot3d.gui.healpix3d.HealSphere;
import healpix.plot3d.gui.healpix3d.RotateAble;

import java.awt.GraphicsConfiguration;

import javax.media.j3d.Alpha;
import javax.media.j3d.AmbientLight;
import javax.media.j3d.Appearance;
import javax.media.j3d.Background;
import javax.media.j3d.BoundingSphere;
import javax.media.j3d.BranchGroup;
import javax.media.j3d.Canvas3D;
import javax.media.j3d.ColoringAttributes;
import javax.media.j3d.LineAttributes;
import javax.media.j3d.PolygonAttributes;
import javax.media.j3d.RotationInterpolator;
import javax.media.j3d.Shape3D;
import javax.media.j3d.Transform3D;
import javax.media.j3d.TransformGroup;
import javax.media.j3d.TransparencyAttributes;
import javax.vecmath.AxisAngle4f;
import javax.vecmath.Color3f;
import javax.vecmath.Point3d;

import com.sun.j3d.utils.behaviors.mouse.MouseRotate;
import com.sun.j3d.utils.behaviors.mouse.MouseTranslate;
import com.sun.j3d.utils.behaviors.mouse.MouseZoom;
import com.sun.j3d.utils.universe.SimpleUniverse;

/**
 * Construct a canvas3D for plotting a sphere from a demo healpix map. Modified
 * version of MapCanvas from G.Giardino. That itself claims to be a "Rehash" of
 * Hipparcos Sky3d.
 *
 * Richard Carr, 6th of May 2004. E. Joliet, 5th of March 2007, modified for
 * Gaia
 *
 * @author ejoliet
 * @version $Id: MapCanvas3D.java 49444 2008-05-07 10:23:02Z ejoliet $
 */

public class MapCanvas3D extends Canvas3D implements RotateAble {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** If nside of map less than this use it. Otherwise use this. */
	public static final int MAX_LONE_FACE_NSIDE = 512;

	/** The Constant FIELD_OF_VIEW_RADIANS. */
	private static final double FIELD_OF_VIEW_RADIANS = 1.2f;

	/** The Constant FRONT_CLIP_DISTANCE. */
	private static final double FRONT_CLIP_DISTANCE = 0.01f;

	/** The Constant EQUATOR_RADIUS. */
	private static final double EQUATOR_RADIUS = 1.1f;

	/** The Constant EQUATOR_RES. */
	private static final int EQUATOR_RES = 40;

	/** The Constant BOUNDINGSPHERE_RADIUS. */
	private static final double BOUNDINGSPHERE_RADIUS = 100.0f;

	/** The Constant ALPHA_INC_DURATION. */
	private static final long ALPHA_INC_DURATION = 4000; // Milliseconds ??

	/** The obj trans. */
	protected TransformGroup objTrans;

	/** The obj scale. */
	protected TransformGroup objScale;

	/** The scene. */
	protected BranchGroup scene;

	/** The grid group. */
	protected BranchGroup axisGroup, equatorGroup, gridGroup; // Optional

	// Items

	// Groups for the modelling of a single face and of the entire sphere.
	// A single face can be shown at higher res without burdening the machine.
	/** The all group. */
	protected BranchGroup loneFaceGroup, allGroup;

	/** The uni. */
	protected SimpleUniverse uni; // = null by default initialization.

	/** The zoom. */
	protected MouseZoom zoom;

	/** The rotator. */
	protected RotationInterpolator rotator;

	/** The equator view. */
	protected boolean axisView = true, equatorView = true;

	/** The all view. */
	protected boolean loneFaceView = false, allView = true;

	/** The nside. */
	protected int nside = 64;

	/** The face no. */
	protected int faceNo; // = 0 by default initialization.

	/** The map. */
	protected HealpixMap theMap;

	/** The show grid. */
	private boolean showGrid = true;

	/**
	 * Just calls parent constructor.
	 *
	 * @param gc
	 *            the GraphicsConfiguration to be used for rendering.
	 */
	public MapCanvas3D(GraphicsConfiguration gc) {
		super(gc);
	}

	/**
	 * From Rotatable interface.
	 *
	 * @return the RotationInterpolator used for the animation.
	 */
	public RotationInterpolator getRotationInterpolator() {
		return rotator;
	}

	/**
	 * Dispose of any old scene and create a new one
	 */
	public void setupScene() {
		if (scene != null) {
			scene.detach();
			scene = null;
		}
		scene = createSceneGraph();
		if (uni == null)
			uni = new SimpleUniverse(this);
	}

	/**
	 * Actually display the already created model
	 */
	public void showScene() {
		scene.compile();
		uni.addBranchGraph(scene);
		// This will move the ViewPlatform back a bit so the
		// objects in the scene can be viewed.
		uni.getViewingPlatform().setNominalViewingTransform();
		uni.getViewer().getView().setFieldOfView(FIELD_OF_VIEW_RADIANS);
		uni.getViewer().getView().setFrontClipDistance(FRONT_CLIP_DISTANCE);
	}

	/**
	 * This sets the nside used for the display of the whole sphere.
	 *
	 * @param nside
	 *            the healpix pixel density parameter.
	 */
	public void setNside(int nside) {
		this.nside = nside;
		if (theMap != null)
			regenerateMaps(); // Condition added by RC, 28/04/2004
	}

	/**
	 * This sets a new map for display.
	 *
	 * @param map
	 *            the new map to display.
	 */
	public void setMap(HealpixMap map) {
		this.theMap = map;

		// If not set up for specific channel, then use first of new map.
		// if(getChannelName() == null)
		// setChannelByName(map.getChannels()[0].getName());
		regenerateMaps();
	}

	/**
	 * This sets a new map for display.
	 *
	 * @return the map currently shown.
	 */
	public HealpixMap getMap() {
		return theMap;
	}

	/**
	 * set up a timer object to drive animation.
	 *
	 * @param alpha
	 *            specifies the timing parameters.
	 */
	public void setAlpha(Alpha alpha) {
		rotator.setAlpha(alpha);
	}

	/**
	 * Specify whether to make axes visible.
	 *
	 * @param b
	 *            whether visible.
	 */
	public void setViewAxis(boolean b) {
		if (b) {
			if (!axisView)
				objTrans.addChild(axisGroup);
		} else {
			if (axisView)
				axisGroup.detach();
		}
		axisView = b;
	}

	/**
	 * Specify whether to make equator visible.
	 *
	 * @param b
	 *            whether visible.
	 */
	public void setViewEquator(boolean b) {
		if (b) {
			if (!equatorView)
				objTrans.addChild(equatorGroup);
		} else {
			if (equatorView)
				equatorGroup.detach();
		}
		equatorView = b;
	}

	/**
	 * Specify whether to make a view of the whole sphere visible.
	 *
	 * @param b
	 *            whether visible.
	 */
	public void setViewWholeSphere(boolean b) {
		if (b) {
			if (allGroup == null && theMap != null)
				setupWholeSphere();
			if (!allView && allGroup != null)
				objTrans.addChild(allGroup);
		} else {
			if (allGroup != null && allView)
				allGroup.detach();
		}
		allView = b;
	}

	/**
	 * Specify whether to make the view of a single face visible.
	 *
	 * @param b
	 *            whether visible.
	 */
	public void setViewSpecificFace(boolean b) {
		if (b) {
			if (loneFaceGroup == null)
				setFace(faceNo);
			if (!loneFaceView && loneFaceGroup != null)
				objTrans.addChild(loneFaceGroup);
		} else {
			if (loneFaceView && loneFaceGroup != null)
				loneFaceGroup.detach();
		}
		loneFaceView = b;

	}

	/**
	 * Modify/specify which single face this canvas will display. Whether i is
	 * actually shown is specified independently.
	 *
	 * @param f
	 *            the face's id.
	 */
	public void setFace(int f) {
		faceNo = f;
	}

	/**
	 * principal method used to configure the java3D model of the sky.
	 *
	 * @return a BranchGroup with the model contents.
	 */
	protected BranchGroup createSceneGraph() {
		// ##### Transform Groups ################
		// Create the transform group node and initialize it to the
		// identity. Enable the TRANSFORM_WRITE capability so tha
		// our behavior code can modify it at runtime.
		objTrans = new TransformGroup();
		objTrans.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
		objTrans.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
		objTrans.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
		objTrans.setCapability(TransformGroup.ALLOW_CHILDREN_WRITE);
		objTrans.setCapability(TransformGroup.ALLOW_CHILDREN_EXTEND);

		// #### Grid ########################################
		if (this.showGrid) {
			gridGroup = new BranchGroup();
			gridGroup.setCapability(BranchGroup.ALLOW_DETACH);
			Group3DSphere sphereRaster = new Group3DSphere(12, 24);
			Appearance sphereRasterApp = new Appearance();
			ColoringAttributes sphereRasterColor = new ColoringAttributes();
			sphereRasterColor.setColor(0.0f, 0.41176f, 0.80f); // royal blue
			sphereRasterApp.setColoringAttributes(sphereRasterColor);
			sphereRaster.setAppearance(sphereRasterApp);
			gridGroup = new BranchGroup();
			gridGroup.addChild(sphereRaster);
			objTrans.addChild(gridGroup);
		}
		// ##### Equator ########################
		equatorGroup = new BranchGroup();
		equatorGroup.setCapability(BranchGroup.ALLOW_DETACH);

		Group3DCircle equator = new Group3DCircle(EQUATOR_RADIUS, EQUATOR_RES);
		Appearance equatorApp = equator.getAppearance();

		ColoringAttributes equatorColor = new ColoringAttributes();
		equatorColor.setColor(new Color3f(java.awt.Color.YELLOW));
		equatorApp.setColoringAttributes(equatorColor);

		LineAttributes eqLine = new LineAttributes();
		eqLine.setLineWidth(3.0f);
		equatorApp.setLineAttributes(eqLine);

		equator.setAppearance(equatorApp);
		equatorGroup.addChild(equator);
		objTrans.addChild(equatorGroup);
		// ######################################

		// ##### Axis ###########################
		axisGroup = new Group3DAxis();
		axisGroup.setCapability(BranchGroup.ALLOW_DETACH);
		objTrans.addChild(axisGroup);
		// ######################################

		objScale = new TransformGroup();
		objScale.addChild(objTrans);

		// ##### root of the branch graph #######
		BranchGroup objRoot = new BranchGroup();
		objRoot.setCapability(BranchGroup.ALLOW_DETACH);
		objRoot.addChild(objScale);
		// ######################################

		BoundingSphere bounds = new BoundingSphere(new Point3d(0.0, 0.0, 0.0),
				BOUNDINGSPHERE_RADIUS);

		// ##### Background #####
		Background bgNode = new Background(new Color3f(java.awt.Color.BLACK));
		bgNode.setApplicationBounds(bounds);
		objRoot.addChild(bgNode);
		// ######################################

		// ##### Lighting #######################
		// Set up the ambient ligh
		AmbientLight ambientLightNode = new AmbientLight();
		ambientLightNode.setInfluencingBounds(bounds);
		objRoot.addChild(ambientLightNode);
		// ######################################
		// ##### Behaviour ######################
		// Create the rotate behavior node
		// MouseRotateY behavior = new MouseRotateY();
		MouseRotate behavior = new MouseRotate();
		behavior.setTransformGroup(objTrans);
		objTrans.addChild(behavior);
		behavior.setSchedulingBounds(bounds);

		// Create the zoom behavior node
		zoom = new MouseZoom();
		zoom.setTransformGroup(objTrans);
		objTrans.addChild(zoom);
		zoom.setSchedulingBounds(bounds);

		// Create the translate behavior node
		MouseTranslate behavior3 = new MouseTranslate();
		behavior3.setTransformGroup(objTrans);
		objTrans.addChild(behavior3);
		behavior3.setSchedulingBounds(bounds);

		// Auto rotator
		Transform3D axis = new Transform3D();
		axis.set(new AxisAngle4f(0, 0, 1, 2)); // theta,x,y,z.
		Alpha mover = new Alpha(0, ALPHA_INC_DURATION); // loop count, period
		rotator = new RotationInterpolator(mover, objTrans);
		rotator.setTransformAxis(axis);
		rotator.setSchedulingBounds(bounds);
		objTrans.addChild(rotator);

		return objRoot;
	}

	/**
	 * Regenerate the BranchGroup modelling a single face, and/or tha modelling
	 * the whole sphere, according to which are visible.
	 */
	protected void regenerateMaps() {
		if (loneFaceView) {
			// setupLoneFace();
		} else {
			loneFaceGroup = null;
		}
		if (allView) {
			setupWholeSphere();
		} else {
			allGroup = null;
		}
	}

	/** set up canvas to display all faces. */
	protected void setupWholeSphere() {

		if (allView && allGroup != null)
			allGroup.detach();

		allGroup = new BranchGroup();
		int nfaces = 12;

		try {
			theMap = theMap.regrade(nside);
		} catch (Exception e) {
			new Exception("Regrading failed", e);
		}

		// Find map min and max
		double min = theMap.getMin();
		double max = theMap.getMax();
		System.out.println("********** #Min(0)=" + min);
		System.out.println("********** #Max(0)=" + max);
		HealpixBase index = null;
		try {
			index = new HealpixBase(theMap.nside(),Scheme.NESTED);
		} catch (Exception e) {
			e.printStackTrace();
		}
		for (int f = 0; f < nfaces; f++) {
			HealSphere face = new DataSphere(f, theMap, 0, min, max, index);
			setAppearance(face);
			allGroup.addChild(face);
		}

		allGroup.setCapability(BranchGroup.ALLOW_DETACH);
		objTrans.addChild(allGroup);
		if (!allView)
			allGroup.detach();
	}

	/**
	 * Set the grid whether it is present or not
	 * @param showGrid1 the grid is added or not
	 */
	public void setShowGrid(boolean showGrid1) {
		this.showGrid = showGrid1;
	}

	/**
	 * Sets the appearance.
	 *
	 * @param shape the new appearance
	 */
	private void setAppearance(Shape3D shape) {
		Appearance sphereApp = new Appearance();
		TransparencyAttributes sphereTrans = new TransparencyAttributes();
		PolygonAttributes spherePa = new PolygonAttributes();

		spherePa.setCullFace(PolygonAttributes.CULL_NONE);
		sphereApp.setPolygonAttributes(spherePa);
		sphereTrans.setTransparency(0.6f);
		sphereTrans.setTransparencyMode(TransparencyAttributes.NICEST);
		sphereApp.setTransparencyAttributes(sphereTrans);
		shape.setAppearance(sphereApp);
	}
}
