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

import java.applet.Applet;
import java.applet.AppletContext;
import java.applet.AppletStub;
import java.applet.AudioClip;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Label;
import java.awt.Menu;
import java.awt.MenuBar;
import java.awt.MenuItem;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.ImageProducer;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JFrame;

/**
 * Run an Applet as an application.
 * <P>
 * Using this class you can add a trivial main program to any Applet and run it
 * directly, as well as from a browser or the appletviewer. And unlike some
 * versions of this concept, MainFrame implements both images and sound.
 * <P>
 * Sample main program: <BLOCKQUOTE>
 * 
 * <PRE>
 * 
 * public static void main( String[] args ) { new Acme.MainFrame( new
 * ThisApplet(), args, 400, 400 ); }
 * 
 * </PRE>
 * 
 * </BLOCKQUOTE> The only methods you need to know about are the constructors.
 * <P>
 * You can specify Applet parameters on the command line, as name=value. For
 * instance, the equivalent of: <BLOCKQUOTE>
 * 
 * <PRE>
 * 
 * &lt;PARAM NAME="pause" VALUE="200"&gt;
 * 
 * </PRE>
 * 
 * </BLOCKQUOTE> would just be: <BLOCKQUOTE>
 * 
 * <PRE>
 * 
 * pause=200
 * 
 * </PRE>
 * 
 * </BLOCKQUOTE> You can also specify three special parameters: <BLOCKQUOTE>
 * 
 * <PRE>
 * 
 * width=N Width of the Applet. height=N Height of the Applet. barebones=true
 * Leave off the menu bar and status area.
 * 
 * </PRE>
 * 
 * </BLOCKQUOTE>
 * <P>
 * <A HREF="/resources/classes/Acme/MainFrame.java">Fetch the software.</A><BR>
 * <A HREF="/resources/classes/Acme.tar.Z">Fetch the entire Acme package.</A>
 */
public class MainFrame extends JFrame implements Runnable, AppletStub,
		AppletContext, ActionListener {
	
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The args. */
	private String[] args = null;

	/** The instances. */
	private static int instances = 0;

	/** The name. */
	private String name;

	/** The barebones. */
	private boolean barebones = false;

	/** The applet. */
	private Applet applet;

	/** The label. */
	private Label label = null;

	/** The applet size. */
	private Dimension appletSize;

	/** The Constant PARAM_PROP_PREFIX. */
	private static final String PARAM_PROP_PREFIX = "parameter.";

	// Constructor with everything specified.
	/**
	 * Instantiates a new main frame.
	 * 
	 * @param applet the applet
	 * @param args the args
	 * @param width the width
	 * @param height the height
	 */
	public MainFrame(Applet applet, String[] args, int width, int height) {
		build(applet, args, width, height);
	}

	// Constructor with no default width/height.
	/**
	 * Instantiates a new main frame.
	 * 
	 * @param applet the applet
	 * @param args the args
	 */
	public MainFrame(Applet applet, String[] args) {
		build(applet, args, -1, -1);
	}

	// Constructor with no arg parsing.
	/**
	 * Instantiates a new main frame.
	 * 
	 * @param applet the applet
	 * @param width the width
	 * @param height the height
	 */
	public MainFrame(Applet applet, int width, int height) {
		build(applet, null, width, height);
	}

	// Internal constructor routine.
	/**
	 * Builds the.
	 * 
	 * @param applet the applet
	 * @param args the args
	 * @param width the width
	 * @param height the height
	 */
	private void build(Applet applet, String[] args, int width, int height) {
		++instances;
		this.applet = applet;
		this.args = args;
		applet.setStub(this);
		name = applet.getClass().getName();
		setTitle(name);

		// Set up properties.
		Properties props = System.getProperties();

		// Turn args into parameters by way of the properties list.
		if (args != null)
			parseArgs(args, props);

		// If width and height are specified in the parameters, override
		// the compiled-in values.
		String widthStr = getParameter("width");
		if (widthStr != null)
			width = Integer.parseInt(widthStr);
		String heightStr = getParameter("height");
		if (heightStr != null)
			height = Integer.parseInt(heightStr);

		// Were width and height specified somewhere?
		if (width == -1 || height == -1) {
			System.err.println("Width and height must be specified.");
			return;
		}

		// Do we want to run bare-bones?
		String bonesStr = getParameter("barebones");
		if (bonesStr != null && bonesStr.equals("true"))
			barebones = true;

		if (!barebones) {
			// Make menu bar.
			MenuBar mb = new MenuBar();
			Menu m = new Menu("Menu");
			m.add(new MenuItem("Restart"));
			m.add(new MenuItem("Quit"));
			mb.add(m);
			setMenuBar(mb);
		}

		// Lay out components.
		setLayout(new BorderLayout());
		add("Center", applet);

		// Set up size.
		pack();
		validate();
		appletSize = applet.getSize();
		applet.setSize(width, height);
		setVisible(true);

		/*
		 * J3D, 98/3/5: Added WindowListener inner class to detect close events.
		 */
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent winEvent) {
				System.exit(0);
			}
		});

		// Start a separate thread to call the applet's init() and start()
		// methods, in case they take a long time.
		(new Thread(this)).start();
	}

	// Turn command-line arguments into Applet parameters, by way of the
	// properties list.
	/**
	 * Parses the args.
	 * 
	 * @param args the args
	 * @param props the props
	 */
	private static void parseArgs(String[] args, Properties props) {
		for (int i = 0; i < args.length; ++i) {
			String arg = args[i];
			int ind = arg.indexOf('=');
			if (ind == -1)
				props.put(PARAM_PROP_PREFIX + arg.toLowerCase(), "");
			else
				props.put(PARAM_PROP_PREFIX
						+ arg.substring(0, ind).toLowerCase(), arg
						.substring(ind + 1));
		}
	}

	// Methods from Runnable.

	// Separate thread to call the applet's init() and start() methods.
	/* (non-Javadoc)
	 * @see java.lang.Runnable#run()
	 */
	public void run() {
		showStatus(name + " initializing...");
		applet.init();
		validate();
		showStatus(name + " starting...");
		applet.start();
		validate();
		showStatus(name + " running...");
	}

	// Methods from AppletStub.
	/* (non-Javadoc)
	 * @see java.awt.Window#isActive()
	 */
	public boolean isActive() {
		return true;
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletStub#getDocumentBase()
	 */
	public URL getDocumentBase() {
		// Returns the current directory.
		String dir = System.getProperty("user.dir");
		String urlDir = dir.replace(File.separatorChar, '/');
		try {
			return new URL("file:" + urlDir + "/");
		} catch (MalformedURLException e) {
			return null;
		}
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletStub#getCodeBase()
	 */
	public URL getCodeBase() {
		// Hack: loop through each item in CLASSPATH, checking if
		// the appropriately named .class file exists there. But
		// this doesn't account for .zip files.
		String path = System.getProperty("java.class.path");
		StringTokenizer st = new StringTokenizer(path, ":");
		while (st.hasMoreElements()) {
			String dir = st.nextToken();
			String filename = dir + File.separatorChar + name + ".class";
			File file = new File(filename);
			if (file.exists()) {
				String urlDir = dir.replace(File.separatorChar, '/');
				try {
					return new URL("file:" + urlDir + "/");
				} catch (MalformedURLException e) {
					return null;
				}
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletStub#getParameter(java.lang.String)
	 */
	public String getParameter(String name) {
		// Return a parameter via the munged names in the properties list.
		return System.getProperty(PARAM_PROP_PREFIX + name.toLowerCase());
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletStub#appletResize(int, int)
	 */
	public void appletResize(int width, int height) {
		// Change the frame's size by the same amount that the applet's
		// size is changing.
		Dimension frameSize = getSize();
		frameSize.width += width - appletSize.width;
		frameSize.height += height - appletSize.height;
		setSize(frameSize);
		appletSize = applet.getSize();
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletStub#getAppletContext()
	 */
	public AppletContext getAppletContext() {
		return this;
	}

	// Methods from AppletContext.

	/* (non-Javadoc)
	 * @see java.applet.AppletContext#getAudioClip(java.net.URL)
	 */
	public AudioClip getAudioClip(URL url) {
		// This is an internal undocumented routine. However, it
		// also provides needed functionality not otherwise available.
		// I suspect that in a future release, JavaSoft will add an
		// audio content handler which encapsulates this, and then
		// we can just do a getContent just like for images.
		return null;
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletContext#getImage(java.net.URL)
	 */
	public Image getImage(URL url) {
		Toolkit tk = Toolkit.getDefaultToolkit();
		try {
			ImageProducer prod = (ImageProducer) url.getContent();
			return tk.createImage(prod);
		} catch (IOException e) {
			return null;
		}
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletContext#getApplet(java.lang.String)
	 */
	public Applet getApplet(String name) {
		// Returns this Applet or nothing.
		if (name.equals(this.name))
			return applet;
		return null;
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletContext#showDocument(java.net.URL)
	 */
	public void showDocument(URL url) {
		// Ignore.
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletContext#showDocument(java.net.URL, java.lang.String)
	 */
	public void showDocument(URL url, String target) {
		// Ignore.
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletContext#showStatus(java.lang.String)
	 */
	public void showStatus(String status) {
		if (label != null)
			label.setText(status);
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletContext#getStream(java.lang.String)
	 */
	public InputStream getStream(String key) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletContext#getStreamKeys()
	 */
	public Iterator<String> getStreamKeys() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletContext#setStream(java.lang.String, java.io.InputStream)
	 */
	public void setStream(String key, InputStream stream) throws IOException {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getActionCommand().equals("Restart")) {
			applet.stop();
			applet.destroy();
			Thread thread = new Thread(this);
			thread.start();
		} else if (evt.getActionCommand().equals("Clone")) {
			try {
				new MainFrame((Applet) applet.getClass().newInstance(), args,
						appletSize.width, appletSize.height);
			} catch (IllegalAccessException e) {
				showStatus(e.getMessage());
			} catch (InstantiationException e) {
				showStatus(e.getMessage());
			}
		} else if (evt.getActionCommand().equals("Close")) {
			setVisible(false);
			remove(applet);
			applet.stop();
			applet.destroy();
			if (label != null)
				remove(label);
			dispose();
			--instances;
			if (instances == 0)
				System.exit(0);
		} else if (evt.getActionCommand().equals("Quit")) {
			System.exit(0);
		}
	}

	/* (non-Javadoc)
	 * @see java.applet.AppletContext#getApplets()
	 */
	public Enumeration<Applet> getApplets() {
		// Just yields this applet.
		Vector<Applet> v = new Vector<Applet>();
		v.addElement(applet);
		return v.elements();
	}
}
