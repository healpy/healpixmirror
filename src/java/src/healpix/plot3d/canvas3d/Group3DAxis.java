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
 */
package healpix.plot3d.canvas3d;

import com.sun.j3d.utils.geometry.Cone;
import com.sun.j3d.utils.geometry.Cylinder;
import com.sun.j3d.utils.geometry.Text2D;

import java.awt.Font;

import javax.media.j3d.Appearance;
import javax.media.j3d.BranchGroup;
import javax.media.j3d.Group;
import javax.media.j3d.Link;
import javax.media.j3d.Material;
import javax.media.j3d.PolygonAttributes;
import javax.media.j3d.SharedGroup;
import javax.media.j3d.Transform3D;
import javax.media.j3d.TransformGroup;

import javax.vecmath.Color3f;
import javax.vecmath.Vector3f;

/**
 * Group3DAxis draws three 3D axes x,y,z, composed of cylinders, with cones as
 * their 'arrow'.
 * 
 * @author ejoliet
 * @version $Id: Group3DAxis.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class Group3DAxis extends BranchGroup {
	
	/** The axis width. */
	private float axisWidth = 0.02f;

	/** The axis height. */
	private float axisHeight = 2.4f;

	/** The cone width. */
	private float coneWidth = 0.04f;

	/** The cone height. */
	private float coneHeight = 0.1f;

	/** The axis cone shift. */
	private float axisConeShift = 1.25f;

	/** The text shift. */
	private float textShift = 1.35f;

	/** The text size. */
	private int textSize = 24;

	/**
	 * Creates a new Group3DAxis object.
	 */
	public Group3DAxis() {
		Color3f red = new Color3f(1.0f, 0.0f, 0.0f);
		SharedGroup axis = buildSharedAxis(red);
		Group allAxis = buildAllAxis(axis, red);
		addChild(allAxis);
	}

	/**
	 * Specify color of the axes
	 */
	public Group3DAxis(Color3f color) {
		SharedGroup axis = buildSharedAxis(color);
		Group allAxis = buildAllAxis(axis, color);
		addChild(allAxis);
	}

	/**
	 * Builds the shared axis.
	 * 
	 * @param color the color
	 * 
	 * @return the shared group
	 */
	private SharedGroup buildSharedAxis(Color3f color) {
		// set material color
		Color3f black = new Color3f(0.0f, 0.0f, 0.0f);
		Appearance axisApp = new Appearance();
		axisApp.setMaterial(new Material(color, black, color, black, 80.0f));

		// the axis is built from a cylinder with a cone tip.
		Cylinder axline = new Cylinder(axisWidth, axisHeight, axisApp);
		Cone axhat = new Cone(coneWidth, coneHeight);
		axhat.setAppearance(axisApp);

		// cone tip translation
		Vector3f trans = new Vector3f(0.0f, axisConeShift, 0.0f);
		Transform3D t3d = new Transform3D();
		TransformGroup tg = new TransformGroup();

		t3d.setTranslation(trans);
		tg.setTransform(t3d);
		tg.addChild(axhat);

		SharedGroup axis = new SharedGroup();
		axis.addChild(axline);
		axis.addChild(tg);
		axis.compile();

		return axis;
	}

	/**
	 * Builds the all axis.
	 * 
	 * @param axis the axis
	 * @param color the color
	 * 
	 * @return the group
	 */
	private Group buildAllAxis(SharedGroup axis, Color3f color) {
		// Using the sharedgroup axis, build 3 axes, with 3 labels
		Group allAxis = new Group();
		double angle = Math.PI / 2.0;
		PolygonAttributes polyAttrib = new PolygonAttributes();
		polyAttrib.setCullFace(PolygonAttributes.CULL_NONE);
		polyAttrib.setBackFaceNormalFlip(true);

		// y axis
		allAxis.addChild(new Link(axis));

		Text2D ylabel = new Text2D("Y", color, "Helvetica", textSize,
				Font.ITALIC);
		Appearance textAppear = ylabel.getAppearance();
		textAppear.setPolygonAttributes(polyAttrib);

		TransformGroup tgYlabel = new TransformGroup();
		Transform3D t3dYlabel = new Transform3D();
		Vector3f ytrans = new Vector3f(0.0f, textShift, 0.0f);
		t3dYlabel.setTranslation(ytrans);
		tgYlabel.setTransform(t3dYlabel);
		tgYlabel.addChild(ylabel);
		allAxis.addChild(tgYlabel);

		// z axis
		TransformGroup tgz = new TransformGroup();
		Transform3D t3dz = new Transform3D();
		t3dz.rotX(angle);
		tgz.setTransform(t3dz);
		tgz.addChild(new Link(axis));
		allAxis.addChild(tgz);

		Text2D zlabel = new Text2D("Z", color, "Helvetica", 24, Font.ITALIC);
		textAppear = zlabel.getAppearance();
		textAppear.setPolygonAttributes(polyAttrib);

		TransformGroup tgZlabelR = new TransformGroup();
		TransformGroup tgZlabelT = new TransformGroup();
		Transform3D t3dZlabelR = new Transform3D();
		Transform3D t3dZlabelT = new Transform3D();
		Vector3f ztrans = new Vector3f(0.0f, 0.0f, textShift);
		t3dZlabelR.rotX(angle);
		t3dZlabelT.setTranslation(ztrans);
		tgZlabelR.setTransform(t3dZlabelR);
		tgZlabelR.addChild(zlabel);
		tgZlabelT.setTransform(t3dZlabelT);
		tgZlabelT.addChild(tgZlabelR);
		allAxis.addChild(tgZlabelT);

		// x axis
		TransformGroup tgx = new TransformGroup();
		Transform3D t3dx = new Transform3D();
		t3dx.rotZ(3 * angle);
		tgx.setTransform(t3dx);
		tgx.addChild(new Link(axis));
		allAxis.addChild(tgx);

		Text2D xlabel = new Text2D("X", color, "Helvetica", 24, Font.ITALIC);
		textAppear = xlabel.getAppearance();
		textAppear.setPolygonAttributes(polyAttrib);

		TransformGroup tgXlabel = new TransformGroup();
		Transform3D t3dXlabel = new Transform3D();
		Vector3f xtrans = new Vector3f(textShift, 0.0f, 0.0f);
		t3dXlabel.setTranslation(xtrans);
		tgXlabel.setTransform(t3dXlabel);
		tgXlabel.addChild(xlabel);
		allAxis.addChild(tgXlabel);

		return allAxis;
	}
}
