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

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.ItemSelectable;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.media.j3d.Alpha;
import javax.media.j3d.RotationInterpolator;
import javax.media.j3d.Transform3D;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.vecmath.AxisAngle4f;


/**
 * The Class RotatePanel.
 */
public class RotatePanel extends JPanel implements ItemListener, ActionListener {
  /**
	 * 
	 */
	private static final long serialVersionUID = 1L;

/** The scene. */
protected RotateAble  scene; 
  
  /** The rot but. */
  protected JButton rotBut;
  
  /** The alpha. */
  protected Alpha alpha;
  
  /** The choice. */
  protected JComboBox choice;
  
  /** The x. */
  JCheckBox x;

/** The y. */
JCheckBox y;

/** The z. */
protected JCheckBox z;
  
  /** initialiser */ 
  protected void init() {
	GridBagLayout gridbag = new GridBagLayout();
    setLayout(gridbag);
	GridBagConstraints c = new GridBagConstraints();
    c.fill = GridBagConstraints.HORIZONTAL;	
    c.gridy = GridBagConstraints.RELATIVE;	
    c.gridx = 0;
    c.ipadx = 0;
    c.ipady = 0;
	//add ( new Label (" "));
	rotBut = new JButton ("Rotate");
	rotBut.setForeground(Color.red);
	rotBut.addActionListener(this);
	choice = new JComboBox();//new Choice();	
	//choice.setForeground(Color.blue);
    choice.addItem("1");
    choice.addItem("2");
    choice.addItem("3");
    choice.addItem("4");
    choice.addItem("5");
    choice.addItem("10");
    choice.addItem("20");
    choice.addItem("30");
    choice.addItem("40");
    choice.addItem("50");
    choice.addItem("100");
    choice.addItem("200");
    choice.addItem("300");
    choice.addItem("400");
    choice.addItem("500");
	choice.setSelectedItem("10");
	choice.addItemListener(this);
	JPanel fac = new JPanel();
	fac.setLayout(new GridLayout(1,2));
	JLabel delay = new JLabel("Delay:");
	delay.setForeground(Color.blue);
	fac.add(delay);
	fac.add(choice);
	//fac.setBackground(Color.white);
	JPanel axis = new JPanel();
	//axis.setBackground(Color.white);
	ButtonGroup cbg = new ButtonGroup();
	x = new JCheckBox("X",false);
	x.setForeground(Color.blue);
	x.addItemListener(this);
	y = new JCheckBox("Y",true);
	y.setForeground(Color.blue);
	y.addItemListener(this);
	z = new JCheckBox("Z",false);
	z.setForeground(Color.blue);
	z.addItemListener(this);
	cbg.add(x);
	cbg.add(y);
	cbg.add(z);
	axis.add(x);	
	axis.add(y);	
	axis.add(z);	
	gridbag.setConstraints(rotBut,c);
	add(rotBut);
	gridbag.setConstraints(axis,c);
	add(axis);
	gridbag.setConstraints(fac,c);
	add(fac);
	//setBackground(Color.white);
	setSpeed(10);
  }
    
	/** Set Star */ 
	public void setScene(RotateAble scene) {
		this.scene = scene;
		//stop();
    }

	/** Set Speed - set alpha factor*/ 
	public void setFactor(int width) {
		/* given widht of fov who slow should we go .. */
	    int factor = 100;	
		if (width < 50 ) factor = 50;
		if (width < 40 ) factor = 40;
		if (width < 30 ) factor = 30;
		if (width < 20 ) factor = 20;
		if (width < 15 ) factor = 10;
		choice.setSelectedItem(""+factor);
		setSpeed(factor);
	}

	/** Set the axis of rotation */ 
	protected void setAxis(int x, int y , int z) {
		if (scene != null)  {
		   RotationInterpolator rot = scene.getRotationInterpolator();
			if (rot !=null) {
				Transform3D axis = new Transform3D();
				AxisAngle4f an = new AxisAngle4f (x,y,z,(float) (Math.PI/2));
				axis.set(an);
				rot.setTransformAxis(axis);//setAxisOfRotation(axis);
			}
		}
	}
	/** Set Speed - set alpha factor*/ 
	protected void setSpeed(int factor) {
		alpha = new Alpha(-1,factor*1000);	
	}
  
  /**
   * Instantiates a new rotate panel.
   */
  public RotatePanel () {
	init();
  }

    /** This is required to implement ActionListener - this gets
        called when someone hits a button */
    public void actionPerformed(ActionEvent e){
         String label= e.getActionCommand();
         if (label=="Rotate") {
			start();
         }
         if (label=="Stop") {
			stop();
         }
	}

	/**
	 * Start.
	 */
	public void start() {
		rotBut.setText("Stop");
		scene.getRotationInterpolator().setAlpha(alpha);
	}
	
	/**
	 * Stop.
	 */
	public void stop() {
		rotBut.setText("Rotate");
		scene.getRotationInterpolator().setAlpha(null);
	}

	   /** Item listener imp  - for choice box */
   public void itemStateChanged(ItemEvent evt) {
		ItemSelectable cb=evt.getItemSelectable();
		if (cb == choice) {
        	int ch=new Integer((String)choice.getSelectedItem()).intValue();
			setSpeed(ch);
			start();
		}
		if (cb == x) {
			setAxis(0,0,1);
		}
		if (cb == y) {
			setAxis(0,1,0);
		}
		if (cb == z) {
			setAxis(1,0,0);
		}

	}
}; 
