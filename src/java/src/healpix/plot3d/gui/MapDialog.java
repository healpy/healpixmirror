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

import java.awt.Button;
import java.awt.Checkbox;
import java.awt.GridLayout;
import java.awt.ItemSelectable;
import java.awt.Panel;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.JFrame;

/**
 * The Class MapDialog.
 */
public class MapDialog extends JFrame implements ActionListener, ItemListener {
    
    /** The Constant serialVersionUID. */
    private static final long serialVersionUID = 1L;

    // protected Criteria crit;
    /** The fwhm. */
    protected RangePanel fwhm;

    /** The nside. */
    protected RangePanel nside;

    /** The maplist. */
    protected java.awt.List maplist;

    /** The map take. */
    protected MapTaker mapTake;

    /** The ignore. */
    protected Checkbox ignore;

    /** The sel. */
    protected Button sel;

    /**
     * Instantiates a new map dialog.
     * 
     * @param mapTake the map take
     */
    public MapDialog(MapTaker mapTake) {
        super("Map Filter");
        this.mapTake = mapTake;
        setSize(300, 320);
        fwhm = new RangePanel("FWHM", "getFWHM", "FWHM",
                "planck.dmci.fullskymap.HealpixMapData");
        nside = new RangePanInt("NSIDE", "getNside", "nside",
                "planck.dmci.fullskymap.HealpixMapData");
        Button close = new Button("Close");
        sel = new Button("Select");
        Button filter = new Button("Filter");
        ignore = new Checkbox("Ignore Criteria", null, false);
        Panel p = new Panel();
        p.add(close);
        p.add(sel);
        sel.setEnabled(false);
        p.add(filter);
        p.add(ignore);
        sel.addActionListener(this);
        filter.addActionListener(this);
        close.addActionListener(this);
        ignore.addItemListener(this);
        maplist = new java.awt.List(1);
        maplist.add("Hit filter");

        Panel critp = new Panel(new GridLayout(2, 1));
        critp.add(fwhm);
        critp.add(nside);
        add("North", critp);
        add(maplist);
        add("South", p);
    }

    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent e) {
        String cmd = e.getActionCommand();
        if (cmd == "Close") {
            setVisible(false);
            return;
        }
        if (cmd == "Filter") {
            // System.out.println(crit);
            try {
                maplist.removeAll();
            } catch (Exception dbe) {
                dbe.printStackTrace();
                maplist.add(" Problem scanning db " + dbe);
            }
            return;
        }
        if (cmd == "Select") {
            String item = maplist.getSelectedItem();
            String id;
            if (item != null)
                id = item.substring(0, (item.indexOf(' ')));
            else
                return;
            try {
                this.setVisible(false);
            } catch (Exception dbe) {
                dbe.printStackTrace();
                maplist.removeAll();
                maplist.add("Can not acess " + id);
            }
            return;
        }

    }

    /* (non-Javadoc)
     * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
     */
    public void itemStateChanged(ItemEvent e) {
        ItemSelectable cb = e.getItemSelectable();
        if (cb == ignore) {
            // crit.setOn(e.getStateChange() != ItemEvent.SELECTED);
            System.out.println("crit ON"
                    + (e.getStateChange() != ItemEvent.SELECTED));
        }
    }
}
