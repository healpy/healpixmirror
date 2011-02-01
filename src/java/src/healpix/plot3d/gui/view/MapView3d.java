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

import healpix.core.dm.HealpixMap;
import healpix.plot3d.canvas3d.ColorBar;
import healpix.plot3d.canvas3d.MapCanvas;
import healpix.plot3d.canvas3d.SineColorTransform;
import healpix.plot3d.gui.BoxDisplay;
import healpix.plot3d.gui.ExtBoxDisplayer;
import healpix.plot3d.gui.ExtractBoxDialog;
import healpix.plot3d.gui.MapTaker;
import healpix.plot3d.gui.healpix3d.RotatePanel;
import healpix.plot3d.gui.healpix3d.SimplePanel;
import healpix.plot3d.gui.util.FitsFileFilter;
import healpix.tools.HealpixMapCreator;

import java.awt.BorderLayout;
import java.awt.Canvas;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.UIManager;

/**
 * Plotting HealpixMap in 3D with Tooltip behaviour and Rotation actions Get
 * data from fits or data model. A HealpixMap can have several data inside
 * (columns/names). See HealpixMap data model interface.
 * 
 * @author ejoliet
 * @version $Id: MapView3d.java 56224 2008-07-30 07:30:00Z ejoliet $
 */
public class MapView3d extends JFrame implements ActionListener, MapTaker {

	/**
	 * Default serial version identifier
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The title to appear in the frame.
	 */
	private static final String FRAME_TITLE = "Healpix 3D Map Viewer";

	/**
	 * The window default WIDTH
	 */
	private static final int FRAME_WIDTH = 800;

	/**
	 * The window default HEIGHT
	 */
	private static final int FRAME_HEIGHT = 800;

	/** The hpan. */
	public SimplePanel hpan;

	/**
	 * Flag indicating if the application has been launched as an applet.
	 */
	private boolean isApplet = false;

	/**
	 * Flag indicating when the user exits the application.
	 */
	private boolean isDone = false;

	/** The sky. */
	private MapCanvas theSky;

	/** The info1. */
	public JLabel info1;

	/** The rot panel. */
	protected RotatePanel rotPanel;

	/** The box dlg. */
	protected ExtractBoxDialog boxDlg;

	/** The displayer. */
	protected ExtBoxDisplayer displayer;

	/** The jf. */
	JFrame jf;

	/** The map. */
	protected HealpixMap theMap;

	/** The fits file filter. */
	protected javax.swing.filechooser.FileFilter fitsFileFilter;

	/** The fch. */
	private JFileChooser fch;

	/** The color bar. */
	private ColorBar colorBar;

	/**
	 * Set up the frame to interact with 3d sphere.
	 */
	public MapView3d(boolean isApplet) {
		super(FRAME_TITLE);
		createCanvas();
		this.isApplet = isApplet;
		setupFrame();
		start();
	}

	/**
	 * 
	 */
	private void createCanvas() {
		theSky = new MapCanvas();
	}

	/**
	 * 
	 */
	private void createCanvas(boolean tooltip, float transp) {
		theSky = new MapCanvas(tooltip, transp);
	}

	/**
	 * Default constructor. Is a frame not an applet.
	 */
	public MapView3d() {
		this(true);
	}

	/**
	 * Set up the frame to interact with 3d sphere.
	 */
	public MapView3d(boolean isApplet, boolean toolTipBehaviour,
			float transpValue) {
		super(FRAME_TITLE);
		createCanvas(toolTipBehaviour, transpValue);
		this.isApplet = isApplet;
		setupFrame();
		start();
	}

	public MapView3d(boolean toolTipBehaviour, float transpValue) {
		this(true, toolTipBehaviour, transpValue);
	}

	/**
	 * Window manager sets the size of the window officially but we can suggest
	 * which size we would like
	 */
	public Dimension getPreferredSize() {
		return new Dimension(750, 680);
	}

	/** set the map dat */
	public void setMap(HealpixMap map) {
		setMap(map, 0);
	}

	/**
	 * set the map dat with ith imap map
	 * 
	 * @param map
	 * @param imap
	 */
	public void setMap(HealpixMap map, int imap) {
		theMap = map;
		colorBar.update(new SineColorTransform(theMap.getMin(imap), theMap
				.getMax(imap)));
		if ( this.isShowing() ) {
			theSky.setMap(theMap, imap);
			theSky.setColorBar(colorBar);
			theSky.setToolTip(false);
			setNameFromMap();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see healpix.plot3d.gui.MapTaker#getMap()
	 */
	public HealpixMap getMap() {
		return theMap;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.Component#setVisible(boolean)
	 */
	public void setVisible(boolean b) {
		if ( b && theMap != null ) {
			theSky.setMap(theMap);
			if ( getMap().getName() != null ) {
				setNameFromMap();
			}
		}
		super.setVisible(b);
	}

	/**
	 * Sets the name from map.
	 */
	private void setNameFromMap() {
		JComboBox jc = new JComboBox(getMap().getName());
		DefaultComboBoxModel aModel = (DefaultComboBoxModel) jc.getModel();
		hpan.colname.setModel(aModel);
	}

	/**
	 * Sets the menubar.
	 * 
	 * @param mb
	 *            the new menubar
	 */
	protected void setMenubar(JMenuBar mb) {
		setJMenuBar(mb);
	}

	/** init lays out the window and sets up components */
	public void start() {
		/*
		 * BorderLayout just makes sure the Object in the Center gets most of
		 * the screen
		 */
		setLayout(new BorderLayout());
		colorBar = new ColorBar();
		// Informationi area where we can put some text ocassionaly
		FlowLayout flNorth = new FlowLayout(FlowLayout.RIGHT, 1, 1);
		JPanel p1 = new JPanel(flNorth);
		info1 = new JLabel("Welcome !");
		// put it along the top
		info1.setAlignmentY(1.0f);// (JLabel.RIGHT);
		add("North", p1);
		displayer = new BoxDisplay();
		// Plotting area - create our own sky class
		// theSky = new GaiaMapCanvas3D(null);
		JPanel jpCanvas = new JPanel();
		jpCanvas.setLayout(new BorderLayout());
		jpCanvas.setOpaque(true);
		jpCanvas.setPreferredSize(new Dimension(600, 600));
		jpCanvas.add(theSky, BorderLayout.CENTER);
		// put it in the center of the window - most space
		add("Center", jpCanvas); // theSky
		theSky.setupScene();
		theSky.showScene();
		jf = new JFrame();
		jf.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		JPanel jpmol = new JPanel();
		jpmol.setLayout(new BorderLayout());
		jpmol.add("Center", (Canvas) displayer);
		jf.add(jpmol);
		// jf.pack();
		// jf.setVisible(true);
		// Create a panel to put the controls in - panel is a long strip
		FlowLayout fl = new FlowLayout(FlowLayout.CENTER, 1, 1);
		@SuppressWarnings("unused")
		JPanel p = new JPanel(fl);

		// Create buttons and add this as Actionlistener - add them to panel
		// JButton quit = new JButton("Close");
		// quit.addActionListener(this);
		// p.add("West", quit);
		// put the panel at the bottom of the screen
		// add("South", p);
		JButton mol = new JButton("Mollweide Proj.");
		mol.addActionListener(this);
		// p1.add(mol);
		p1.add(info1);
		// p.add("West", mol);
		// put the panel at the bottom of the screen
		// add("South", p);
		theSky.setColorBar(colorBar);
		add("South", colorBar);
		JPanel rp = new JPanel();
		rp.setBackground(Color.black);

		// rp.setLayout(new GridLayout(3,1));
		GridBagLayout gridbag = new GridBagLayout();
		rp.setLayout(gridbag);
		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.BOTH;
		cons.gridy = GridBagConstraints.RELATIVE;
		cons.gridx = 0;
		cons.ipadx = 0;
		cons.ipady = 10;

		hpan = new SimplePanel(theSky);
		gridbag.setConstraints(hpan, cons);
		rp.add(hpan);
		// Create control panel for rotations
		rotPanel = new RotatePanel();
		gridbag.setConstraints(rotPanel, cons);
		rp.add(rotPanel);
		// attach the rotPanel panel to the sky
		rotPanel.setScene(theSky);
		// add to right of screen
		this.add("East", rp);
		rotPanel.start();
		fitsFileFilter = new FitsFileFilter();
		fch = new JFileChooser();
		fch.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fch.setFileFilter(fitsFileFilter);
		setJMenuBar(getMenubar());
		pack();
		// setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		// setVisible(true);

	};

	/**
	 * Gets the menubar.
	 * 
	 * @return the menubar
	 */
	private JMenuBar getMenubar() {
		JMenuBar mb = new JMenuBar();
		JMenu menu = new JMenu("File");
		JMenuItem it = new JMenuItem("Open...");
		it.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
				ActionEvent.ALT_MASK));
		it.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				if ( fch.showOpenDialog(null) == JFileChooser.APPROVE_OPTION ) {
					if ( fch.getSelectedFile() != null ) {
						try {
							HealpixMapCreator cr = new HealpixMapCreator(fch
									.getSelectedFile().getAbsolutePath());
							setMap(cr.getMap());
							info1.setText(fch.getSelectedFile()
									.getAbsolutePath()
									+ " [Nside=" + getMap().nside() + "]");
						} catch ( Exception ex ) {
							JOptionPane
									.showMessageDialog(getContentPane(),
											"Format couldn't be read, not implemented!");
						}
					}
				}
			}
		});
		menu.add(it);
		it = new JMenuItem("Save as...");
		it.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
				ActionEvent.ALT_MASK));
		it.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				if ( fch.showSaveDialog(null) == JFileChooser.APPROVE_OPTION ) {
					if ( fch.getSelectedFile() != null ) {
						// info1.setText(fch.getSelectedFile().getAbsolutePath());
						info1.setText(fch.getSelectedFile().getAbsolutePath()
								+ " [Nside=" + getMap().nside() + "]");
						try {
							getMap().toDataSet(
									fch.getSelectedFile().getAbsolutePath());
						} catch ( Exception e1 ) {
							e1.printStackTrace();
							JOptionPane.showMessageDialog(getContentPane(), e1
									.getMessage());
						}
					}
				}
			}
		});
		menu.add(it);
		mb.add(menu);
		return mb;
	}

	/**
	 * This is required to implement ActionListener - this gets called when
	 * someone hits a button
	 */
	public void actionPerformed(ActionEvent e) {
		String label = e.getActionCommand();
		if ( label.equals("Close") ) {
			setVisible(false);
			return;
		} else if ( label.equals("Mollweide Proj.") ) {
			if ( boxDlg == null ) {
				boxDlg = new ExtractBoxDialog(displayer);
			}
			boxDlg.setMap(getMap());
			jf.pack();
			jf.setVisible(true);
			boxDlg.setVisible(true);
		}
	}

	/**
	 * Gets the the sky.
	 * 
	 * @return the the sky
	 */
	public MapCanvas getTheSky() {
		return this.theSky;
	}

	/**
	 * Sets the path.
	 * 
	 * @param name
	 *            the new path
	 */
	public void setPath(String name) {
		fch.setCurrentDirectory(new File(name));
	}

	/**
	 * Setup the window and frame options, set the size,...
	 */
	private void setupFrame() {

		// Set cross-platform Java L&F (also called "Metal")
		try {
			UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
		} catch ( Exception e ) {
			new Exception("Failed to set Metal Look and Feel" + e);
		}

		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				exitApplication();
			}
		});

		int width = Toolkit.getDefaultToolkit().getScreenSize().width;
		int height = Toolkit.getDefaultToolkit().getScreenSize().height;
		int frameWidth = Math.min(FRAME_WIDTH, width - 100);
		int frameHeight = Math.min(FRAME_HEIGHT, height - 100);
		setSize(new Dimension(frameWidth, frameHeight));
		setLocation(( width - frameWidth ) / 2, ( ( height - frameHeight ) / 2 ));
	}

	/**
	 * Method to exit the application, set the isDone flag to true.
	 */
	private void exitApplication() {

		setVisible(false);
		isDone = true;
		if ( !isApplet ) {
			System.exit(0);
		}
	}

	/**
	 * Tells if the application has finished.
	 * 
	 * @return true if finished, false otherwise
	 */
	public boolean isDone() {
		return isDone;
	}
}
