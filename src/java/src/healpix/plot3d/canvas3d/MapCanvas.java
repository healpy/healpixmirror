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

import healpix.core.dm.HealpixMap;
import healpix.plot3d.gui.healpix3d.DataSphere;
import healpix.plot3d.gui.healpix3d.HealSphere;
import healpix.plot3d.gui.healpix3d.RotateAble;

import javax.media.j3d.Alpha;
import javax.media.j3d.AmbientLight;
import javax.media.j3d.Appearance;
import javax.media.j3d.BoundingSphere;
import javax.media.j3d.BranchGroup;
import javax.media.j3d.Canvas3D;
import javax.media.j3d.ColoringAttributes;
import javax.media.j3d.DirectionalLight;
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
import javax.vecmath.Vector3f;

import com.sun.j3d.utils.behaviors.mouse.MouseRotate;
import com.sun.j3d.utils.behaviors.mouse.MouseTranslate;
import com.sun.j3d.utils.behaviors.mouse.MouseZoom;
import com.sun.j3d.utils.universe.SimpleUniverse;

/**
 * Create Canvas 3D with tooltip behaviour and Color bar min/max displayed.
 * Tooltip behaviour added to actually get info from a data sphere face.
 * Transparency feature is present to give more interaction to the scene.
 * Extends the code originally from G. Giardino. That itself claims to be a
 * "Rehash" of Hipparcos Sky3d. Added features like tooltip and transparency
 * function.
 * 
 * @author ejoliet
 * @version $Id: MapCanvas.java 124840 2010-02-19 22:01:41Z ejoliet $
 */
public class MapCanvas extends Canvas3D implements RotateAble {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The tool tip behavior. */
	ToolTipBehavior toolTipBehavior;

	/** The obj trans. */
	protected TransformGroup objTrans;

	/** The obj scale. */
	protected TransformGroup objScale;

	/** The bounds. */
	BoundingSphere bounds;

	/** The scene. */
	protected BranchGroup scene;

	/** The colorbargroup. */
	protected BranchGroup colorbargroup;

	/** The axis group. */
	protected BranchGroup axisGroup; // axis

	/** The equator group. */
	protected BranchGroup equatorGroup; // equator

	/** The grid group. */
	protected BranchGroup gridGroup; // equator

	/** The all group. */
	protected BranchGroup allGroup; // all healpix sphere

	/** The nest group. */
	protected BranchGroup nestGroup; // a face of the healpix sphere - bad

	// name
	/** The uni. */
	protected SimpleUniverse uni = null;

	/** The zoom. */
	protected MouseZoom zoom;

	/** The rotator. */
	protected RotationInterpolator rotator;

	/** The nest view. */
	protected boolean nestView = false;

	/** The axis view. */
	protected boolean axisView = true;

	/** The equator view. */
	protected boolean equatorView = true;

	/** The all view. */
	protected boolean allView = true;

	/** The nside. */
	protected int nside = 64;

	/** The zone no. */
	protected int zoneNo = 3;

	/** The face no. */
	protected int faceNo = 0;

	/** The map. */
	public HealpixMap theMap;

	/** The imap. */
	private int imap = 0; // default map

	/** The colname sel. */
	private String colnameSel;

	/** The colorbar. */
	private ColorBar colorbar;

	/** The show tooltip. */
	private boolean showTooltip = false;

	/** The show grid. */
	private boolean showGrid = true;

	/** The transparency value. */
	private float transparencyValue = 0.2f;

	/** The face. */
	private HealSphere[] face;

	/** The sphere app. */
	private Appearance sphereApp;

	/**
	 * Creates a new MapCanvas object.
	 */
	public MapCanvas() {
		super(SimpleUniverse.getPreferredConfiguration());
		// Apperance for Stars
	}

	/**
	 * @param tooltip
	 * @param transp
	 */
	public MapCanvas(boolean tooltip, float transp) {
		super(SimpleUniverse.getPreferredConfiguration());
		this.showTooltip = tooltip;
		this.transparencyValue = transp;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see healpix.plot3d.gui.healpix3d.RotateAble#getRotationInterpolator()
	 */
	public RotationInterpolator getRotationInterpolator() {
		return rotator;
	}

	/**
	 * Creates the scene graph.
	 * 
	 * @return the branch group
	 */
	public BranchGroup createSceneGraph() {
		// Create the root of the branch graph
		BranchGroup objRoot = new BranchGroup();
		objRoot.setCapability(BranchGroup.ALLOW_DETACH);

		axisGroup = new Group3DAxis();
		equatorGroup = new BranchGroup(); // The sphere equator

		// Create the transform group node and initialize it to the
		// identity. Enable the TRANSFORM_WRITE capability so that
		// our behavior code can modify it at runtime. Add it to the
		// root of the subgraph.
		objScale = new TransformGroup();
		objTrans = new TransformGroup();
		axisGroup.setCapability(BranchGroup.ALLOW_DETACH);
		equatorGroup.setCapability(BranchGroup.ALLOW_DETACH);

		objTrans.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
		objTrans.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
		objTrans.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
		objTrans.setCapability(TransformGroup.ALLOW_CHILDREN_WRITE);
		objTrans.setCapability(TransformGroup.ALLOW_CHILDREN_EXTEND);
		objScale.addChild(objTrans);
		objRoot.addChild(objScale);

		bounds = new BoundingSphere(new Point3d(0.0, 0.0, 0.0), 100.0);

		// ----------------
		// Generate Equator Branch
		// ----------------
		Group3DCircle equator = new Group3DCircle(1.1, 40);
		Appearance equatorApp = equator.getAppearance();
		ColoringAttributes equatorColor = new ColoringAttributes();
		LineAttributes eqLine = new LineAttributes();
		eqLine.setLineWidth(3.0f);
		equatorColor.setColor(1.0f, 0.0f, 0.0f); // yellow
		equatorApp.setColoringAttributes(equatorColor);
		equatorApp.setLineAttributes(eqLine);
		equatorGroup.addChild(equator);

		// -------------
		// Generate grid
		// -------------
		gridGroup = new BranchGroup();
		gridGroup.setCapability(BranchGroup.ALLOW_DETACH);

		Group3DSphere sphereRaster = new Group3DSphere(12, 24);
		Appearance sphereRasterApp = new Appearance();
		ColoringAttributes sphereRasterColor = new ColoringAttributes();
		sphereRasterColor.setColor(0.0f, 0.41176f, 0.80f); // royal blue
		sphereRasterApp.setColoringAttributes(sphereRasterColor);
		sphereRaster.setAppearance(sphereRasterApp);
		gridGroup.addChild(sphereRaster);

		objTrans.addChild(gridGroup);
		objTrans.addChild(equatorGroup);
		objTrans.addChild(axisGroup);

		// Set up the ambient light
		AmbientLight ambientLightNode = new AmbientLight();
		ambientLightNode.setInfluencingBounds(bounds);
		objRoot.addChild(ambientLightNode);

		// Set up the directional lights
		Color3f light1Color = new Color3f(0.8f, 0.8f, 0.8f);
		Vector3f light1Direction = new Vector3f(4.0f, -7.0f, -12.0f);
		Color3f light2Color = new Color3f(0.3f, 0.3f, 0.3f);
		Vector3f light2Direction = new Vector3f(-6.0f, -2.0f, -1.0f);

		DirectionalLight light1 = new DirectionalLight(light1Color,
				light1Direction);
		light1.setInfluencingBounds(bounds);
		objRoot.addChild(light1);

		DirectionalLight light2 = new DirectionalLight(light2Color,
				light2Direction);
		light2.setInfluencingBounds(bounds);
		objRoot.addChild(light2);

		// Create the rotate behavior node
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
		Alpha mover = new Alpha(0, 4000);
		rotator = new RotationInterpolator(mover, objTrans);
		rotator.setSchedulingBounds(bounds);
		objTrans.addChild(rotator);

		Transform3D axis = new Transform3D();
		axis.set(new AxisAngle4f(1, 0, 0, 0));
		rotator.setTransformAxis(axis);

		return objRoot;
	}

	/**
	 * Set the map on sphere with a particular nside
	 * 
	 * @param nside
	 *            healpix map nside resolution number
	 */
	protected void makeMap(int nside) {
		this.nside = nside;

		if ( nestView ) {
			setFace(faceNo);
		} else {
			nestGroup = null;
		}

		if ( allView ) {
			setAll();
		} else {
			allGroup = null;
		}
	}

	/**
	 * Set the scene if not before
	 */
	public void setupScene() {
		if ( scene != null ) {
			scene.detach();
			scene = null;
		}

		scene = createSceneGraph();
		toolTipBehavior = new ToolTipBehavior(this, scene, bounds);
		toolTipBehavior.setEnable(true);
		toolTipBehavior.setCanShowToolTips(false);
		addColorBar();

		if ( uni == null ) {
			uni = new SimpleUniverse(this);
		}
	}

	/**
	 * Add the color bar to the viewer
	 */
	private void addColorBar() {
		if ( scene != null ) {
			colorbargroup = new BranchGroup();

			scene.addChild(colorbargroup);
		}
	}

	/**
	 * Actually shows the scene with the sphere
	 */
	public void showScene() {
		scene.compile();
		uni.addBranchGraph(scene);
		// This will move the ViewPlatform back a bit so the
		// objects in the scene can be viewed.
		uni.getViewingPlatform().setNominalViewingTransform();
		uni.getViewer().getView().setFieldOfView(1.2);
		uni.getViewer().getView().setFrontClipDistance(0.01);
	}

	/**
	 * Set the resolution number of the {@link HealpixMap} displayed
	 * 
	 * @param nside
	 *            resolution number
	 */
	public void setNside(int nside) {
		makeMap(nside);
	}

	/**
	 * Set the displayed {@link HealpixMap}
	 * 
	 * @param map
	 *            {@link HealpixMap}
	 */
	public void setMap(HealpixMap map, int ithmap) {
		this.theMap = map;
		this.imap = ithmap;
		makeMap(theMap.nside());
	}

	/**
	 * Set the displayed {@link HealpixMap}
	 * 
	 * @param map
	 *            {@link HealpixMap}
	 */
	public void setMap(HealpixMap map) {
		setMap(map, 0);
	}

	/**
	 * Set the color bar to be displayed
	 * 
	 * @param cb
	 *            the {@link ColorBar} object
	 */
	public void setColorBar(ColorBar cb) {
		this.colorbar = cb;
	}

	/**
	 * Gets the {@link ColorBar}
	 * 
	 * @return {@link ColorBar}
	 */
	public ColorBar getColorBar() {
		return colorbar;
	}

	/**
	 * Specify whether to make axis visible.
	 * 
	 * @param b
	 *            axis is displayed if true
	 */
	public void setViewAxis(boolean b) {
		if ( b ) {
			if ( !axisView ) {
				objTrans.addChild(axisGroup);
			}
		} else {
			if ( axisView ) {
				axisGroup.detach();
			}
		}

		axisView = b;
	}

	/**
	 * Specify whether to make grid visible.
	 * 
	 * @param b
	 *            whether visible.
	 */
	public void setViewGrid(boolean b) {
		if ( b ) {
			if ( !showGrid ) {
				objTrans.addChild(gridGroup);
			}
		} else {
			if ( showGrid ) {
				gridGroup.detach();
			}
		}

		showGrid = b;
	}

	/**
	 * Specify whether to make equator visible.
	 * 
	 * @param b
	 *            equator is shown if true
	 */
	public void setViewEquator(boolean b) {
		if ( b ) {
			if ( !equatorView ) {
				objTrans.addChild(equatorGroup);
			}
		} else {
			if ( equatorView ) {
				equatorGroup.detach();
			}
		}

		equatorView = b;
	}

	/**
	 * Specify whether to make all scene visible.
	 * 
	 * @param b
	 *            scene is shown if true
	 */
	public void setViewAll(boolean b) {
		if ( b ) {
			if ( allGroup == null ) {
				setAll();
			}

			if ( !allView ) {
				objTrans.addChild(allGroup);
			}
		} else {
			if ( ( allGroup != null ) && allView ) {
				allGroup.detach();
			}
		}

		allView = b;
	}

	/**
	 * Specify whether to make face visible
	 * 
	 * @param b
	 *            face displayed only if true
	 */
	public void setViewNest(boolean b) {
		if ( b ) {
			if ( nestGroup == null ) {
				setFace(faceNo);
			}

			if ( !nestView ) {
				setFace(faceNo);
				objTrans.addChild(nestGroup);
			}
		} else {
			if ( nestView && ( nestGroup != null ) ) {
				nestGroup.detach();
			}
		}

		nestView = b;
	}

	/**
	 * Set the face to be displayed
	 * 
	 * @param f
	 *            face number
	 */
	public void setFace(int f) {
		faceNo = f;

		if ( nestView && ( nestGroup != null ) ) {
			nestGroup.detach();
		}

		nestGroup = new BranchGroup();

//		if (64 < theMap.nside()) {
//			this.nside=64;
		try {
			// rch = (Fchannel) ch.regrade(nside);
			theMap = theMap.regrade(nside);
		} catch ( Exception e ) {
			e.printStackTrace();
		}

		// double max = Double.MIN_VALUE;
		// double min = Double.MAX_VALUE;
		// int start = faceNo * nside * nside;
		// int end = start + (nside * nside);

		// for (int i = start; i < end; i++) {
		// // double val = rch.getPixel(i);
		// double val = (double) theMap.get(this.imap, i);
		//
		// if (val < min) {
		// min = val;
		// }
		//
		// if (val > max) {
		// max = val;
		// }
		// }

		System.out.println("Face, MapCanvas Imap:" + this.imap);

		HealSphere face = new DataSphere(f, theMap, this.imap);
		setAppearance(false, face);
		nestGroup.addChild(face);
		nestGroup.setCapability(BranchGroup.ALLOW_DETACH);
		objTrans.addChild(nestGroup);

		if ( !nestView ) {
			nestGroup.detach();
		}
	}

	/**
	 * Set the scene with the ith data map from {@link HealpixMap} loaded
	 * 
	 * @param imap
	 *            ith map in {@link HealpixMap} loaded
	 */
	public void setAll(int imap) {
		this.imap = imap;
		setAll();
	}

	/**
	 * Set the scene
	 */
	public void setAll() {
		if ( allView && ( allGroup != null ) ) {
			allGroup.detach();
		}

		allGroup = new BranchGroup();

		int nfaces = 12;
		face = new HealSphere[nfaces];

		if(nside > 128)		
			nside = 128;
		
		try {
			theMap = theMap.regrade(nside);
		} catch ( Exception e ) {
			e.printStackTrace();
		}

		// finding channel min and max
		// double max = Double.MIN_VALUE;
		// double min = Double.MAX_VALUE;
		// int npix = 12 * nside * nside;
		System.out.println("MapCanvas Imap:" + imap);
		System.out.println("MapCanvas Nside:" + nside);

		// for (int i = 0; i < npix; i++) {
		// // double val = rch.getPixel(i);
		// double val = (double) theMap.get(imap, i);
		//
		// if (val < min) {
		// min = val;
		// }
		//
		// if (val > max) {
		// max = val;
		// }
		// }
		//
		// System.out.println("MapCanvas Min/Max:" + min + "/" + max);
		System.out.println("MapCanvas Min/Max:" + theMap.getMin(imap) + "/"
				+ theMap.getMax(imap));

		for ( int f = 0; f < nfaces; f++ ) {
			this.face[f] = new DataSphere(f, theMap, imap); // last

			this.face[f].setAppearance(getFaceAppearance());
			this.face[f]
					.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_WRITE);
			this.face[f]
					.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);
			this.face[f].setCapability(PolygonAttributes.ALLOW_CULL_FACE_WRITE);
			this.face[f].setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);

			this.face[f]
					.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_WRITE);
			this.face[f].setCapability(PolygonAttributes.ALLOW_CULL_FACE_WRITE);
			this.face[f].setCapability(Shape3D.ALLOW_APPEARANCE_OVERRIDE_WRITE);
			face[f].setCapability(BranchGroup.ALLOW_DETACH);
			setAppearance(showGrid, face[f]);
			allGroup.addChild(face[f]);
		}

		allGroup.setCapability(BranchGroup.ALLOW_DETACH);
		objTrans.addChild(allGroup);
		toolTipBehavior.setEnable(true);
		toolTipBehavior.setCanShowToolTips(showTooltip);

		if ( !allView ) {
			allGroup.detach();
		}
		if (this.colorbar != null) {
			this.colorbar.update(new SineColorTransform(theMap.getMin(imap),
					theMap.getMax(imap)));
		}
	}

	/**
	 * Get the face appearance properties
	 * 
	 * @return {@link Appearance} of the face
	 */
	private Appearance getFaceAppearance() {
		Appearance sphereApp = new Appearance();
		TransparencyAttributes sphereTrans = new TransparencyAttributes();
		sphereTrans.setTransparency(transparencyValue);
		sphereTrans.setTransparencyMode(TransparencyAttributes.NICEST);
		sphereApp.setTransparencyAttributes(sphereTrans);

		PolygonAttributes spherePa = new PolygonAttributes();
		spherePa.setCullFace(showTooltip ? PolygonAttributes.CULL_BACK
				: PolygonAttributes.CULL_NONE);
		spherePa.setCapability(PolygonAttributes.ALLOW_CULL_FACE_WRITE);
		sphereApp.setPolygonAttributes(spherePa);
		sphereApp.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);
		sphereApp.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_WRITE);
		sphereApp.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);

		return sphereApp;
	}

	/**
	 * Set the tooltip beaviour state
	 * 
	 * @param isTrue
	 *            tooltip will be shown if true
	 */
	public void setToolTip(boolean isTrue) {
		showTooltip = isTrue;

		// for ( int f = 0; f < 12; f++ ) {
		// face[f].setAppearance(getFaceAppearance());
		// }

		toolTipBehavior.setCanShowToolTips(isTrue);
	}

	/**
	 * Gets the state of tootlip behaviour
	 * 
	 * @return true if tooltip has been enabled
	 */
	public boolean isToolTipEnabled() {
		return this.showTooltip;
	}

	/**
	 * Set the map called from its name tmp
	 * 
	 * @param tmp
	 *            map name
	 */
	public void setColname(String tmp) {
		colnameSel = tmp;
		this.imap = this.theMap.getImap(colnameSel);
		theMap.setImap(imap);
		getColorBar()
				.update(
						new SineColorTransform(theMap.getMin(imap), theMap
								.getMax(imap)));
		System.out.println("Imap=" + imap);
		setFace(faceNo);
		setAll(imap);
	}

	/**
	 * Apply the transparency appearance properties to a shape
	 * {@link HealSphere}
	 * 
	 * @param showGrid2
	 *            the grid will be visible if true
	 * @param shape
	 *            {@link HealSphere} to apply transparency appearance
	 */
	private void setAppearance(boolean showGrid2, HealSphere shape) {
		sphereApp = new Appearance();

		// sphereApp.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_WRITE);
		TransparencyAttributes sphereTrans = new TransparencyAttributes();
		PolygonAttributes spherePa = new PolygonAttributes();

		spherePa.setCullFace(showGrid2 ? PolygonAttributes.CULL_BACK
				: PolygonAttributes.CULL_NONE);
		sphereApp.setPolygonAttributes(spherePa);
		sphereTrans.setTransparency(transparencyValue);
		sphereTrans.setTransparencyMode(TransparencyAttributes.NICEST);
		sphereApp.setTransparencyAttributes(sphereTrans);
		shape.setAppearance(sphereApp);
	}

	/**
	 * Set the transparency factor, representing the percent of tranparency. 0
	 * is totally opaque -no tranparency-. 1 is totally transparent
	 * 
	 * @param val
	 *            percent value of transparency (0.->1.)
	 */
	public void setTransparency(float val) {
		this.transparencyValue = val;
	}

	/**
	 * Gets tranparency value
	 * 
	 * @return tranparency (%) value
	 */
	public float getTransparency() {
		return this.transparencyValue;
	}

	/**
	 * Update faces.
	 */
	public void updateFaces() {
		if ( face != null ) {
			for ( int f = 0; f < face.length; f++ ) {
				face[f].setAppearance(new Appearance());
				face[f].setAppearance(getFaceAppearance());
			}
		}
	}
}
