<?php require('_header.inc.php'); ?>




<h2>Downloading HEALPix</h2>

<p>
The HEALPix software can be downloaded freely, without registration.
However, if you wish to be kept informed of HEALPix developments, updates and new releases, 
please subscribe to the moderated <tt>Healpix-users</tt> mailing list by filling in <a href="https://lists.sourceforge.net/lists/listinfo/healpix-users">its web form</a>, <em>or</em> by sending to 

<script language=javascript><!--
var p1 = 'ma'; var n1 = 'healpix&#045;'; var s1 = '&#063;'; var p2 = 'ilto&'; var n2 = 'users&#045;'; var s2 = 'subject&#061;'; var p3 = '#58;'; var n3 = 'request&#64;'; var s3 = 'subscribe'
var n5 = 'sourceforge&#046;'; var n4 = 'lists&#046;'; var n6 = 'net';
  document.write('<a href='+p1+p2+p3+n1+n2+n3+n4+n5+n6+s1+s2+s3+'>'+n1+n2+n3+n4+n5+n6+'</a>')
    --></script><noscript><em>[Enable javascript in your browser to see the email address]</em></noscript>

an empty email with only &quot;<tt>subscribe</tt>&quot; on the Subject line.
</p>
<!-- provide your name and e-mail address below:
</p>

			<form action="/cgi-bin/register.cgi">
<table width=100% border=0 cellspacing=0 cellpadding=2>
  <tr align=center valign=middle>
    <td width=50%>
			First Name:<br>
			<input name="fnam" value="" type="text" size="32" />
    </td>
    <td width=50%>
			Last Name:<br>
			<input name="lnam" value="" type="text" size="32" />
			
    </td>
  </tr>

  <tr align=center valign=middle>
    <td colspan=2 width=50%>
			E-mail (required):<br>
			<input name="eadd" value="" type="text" size="32" />
    </td>
  </tr>
  <tr align=center valign=middle>
    <td colspan=2 width=50%>			
			<input class="button" type="submit" value="Subscribe"/>
         		<input class="button" type="reset"  value="  Reset  "/>	
    </td>
  </tr>
</table>			
			</form>	
-->
			
<p>		
<A name="top"></A>
The recommended channel for obtaining and installing the latest HEALPix software is to download the <em>source</em> package from the <a href="http://sourceforge.net/projects/healpix/">SourceForge Download page</a> under the GNU General Public License, version2 <a href="http://www.gnu.org/licenses/gpl-2.0.html">(GPLv2)</a>, and follow the <a href="documentation.php">installation directions</a>.<br>
Prepackaged or precompiled HEALPix packages can also be found in various places, with variable delays, for e.g. MacOSX (<a href="http://www.macports.org/ports.php?by=name&substr=healpix">MacPorts</a>, <a href="http://braumeister.org/repos/Homebrew/homebrew-science/formula/healpix">HomeBrew</a>) and Linux (<a href="http://fr2.rpmfind.net/linux/rpm2html/search.php?query=healpix&submit=Search+...">RPM</a>) systems, under the <a href="http://www.gnu.org/licenses/gpl-2.0.html">same licensing scheme</a>.
</p>

<p>
Use of the HEALPix software package <em>in any form</em> should be explicitly acknowledged in all publications in the following form:
              <ul>
                <li>
                  an acknowledgment statement &ndash; "Some of the results in this paper have been derived using the HEALPix (<a href="http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=2005ApJ...622..759G&amp;db_key=AST&amp;high=41069202cf02947">K.M. G&oacute;rski et al., 2005, ApJ, 622, p759</a>) package", and
                </li><br>
                <li>
                  at the first use of the HEALPix acronym, a footnote placed in the main body of the paper referring to the HEALPix website &ndash; currently <a href="http://healpix.sourceforge.net">http://healpix.sourceforge.net</a>
                </li>
</ul>
</p>


<!-- ******************************************************  -->
<!-- ******************************************************  -->


<h2>New Features in HEALPix</h2>
<P>
<!--Table of Child-Links-->
<A NAME="tocl"></A>

<UL CLASS="ChildLinks">

<LI><A NAME="tex2html_320_12"
  HREF="#SECTION_320_10000000000000000">Bug corrections and Improvements in Version 3.20</A>
<UL>
<LI><A NAME="tex2html_320_13"
  HREF="#SECTION_320_11000000000000000">General</A>
<LI><A NAME="tex2html_320_14"
  HREF="#SECTION_320_12000000000000000">C</A>
<LI><A NAME="tex2html_320_15"
  HREF="#SECTION_320_13000000000000000">C++</A>
<LI><A NAME="tex2html_320_16"
  HREF="#SECTION_320_14000000000000000">Fortran</A>
<LI><A NAME="tex2html_320_17"
  HREF="#SECTION_320_15000000000000000">IDL</A>
<LI><A NAME="tex2html_320_18"
  HREF="#SECTION_320_16000000000000000">Java</A>
<LI><A NAME="tex2html_320_19"
  HREF="#SECTION_320_17000000000000000">Python</A>
</UL>

<LI><A NAME="tex2html22"
  HREF="#SECTION_311_00010000000000000000">Bug corrections and Improvements in Version 3.11</A>
<UL>
<LI><A NAME="tex2html23"
  HREF="#SECTION_311_00011000000000000000">General</A>
<LI><A NAME="tex2html24"
  HREF="#SECTION_311_00012000000000000000">C++</A>
<LI><A NAME="tex2html25"
  HREF="#SECTION_311_00013000000000000000">Fortran</A>
<LI><A NAME="tex2html26"
  HREF="#SECTION_311_00014000000000000000">IDL</A>
</UL>

</ul>
<!--End of Table of Child-Links-->
</p>

<!-- ************************************************* -->



<h3><A NAME="SECTION_320_10000000000000000">
New Features in HEALPix 3.20</A>
</h3>

<h4><A NAME="SECTION_320_11000000000000000">
General</A>
</h4>
	
<UL>
<LI>Generation of 
<A NAME="tex2html_320_1"
  HREF="http://en.wikipedia.org/wiki/Pkg-config"><TT>pkg-config</TT></A>
files during the configuration of the C, C++ and F90 packages. 
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>

<h4><A NAME="SECTION_320_12000000000000000">
C</A>
</h4>

<UL>
<LI>Top <TT>configure</TT> script now proposes compilation with <EM>or</EM> without
CFITSIO-related functions
</LI>
<LI>Improved <TT>autotools</TT> support
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>

<P>

<h4><A NAME="SECTION_320_13000000000000000">
C++</A>
</h4>

<UL>
<LI>automatic workaround for bugs in older versions of GNU <TT>g++</TT> compiler
 (bug reports 
<A NAME="tex2html_320_2"
  HREF="http://sourceforge.net/p/healpix/bugs/37">37</A>, 
<A NAME="tex2html_320_3"
  HREF="http://sourceforge.net/p/healpix/bugs/45">45</A>, 
<A NAME="tex2html_320_4"
  HREF="http://sourceforge.net/p/healpix/bugs/48">48</A>, 
<A NAME="tex2html_320_5"
  HREF="http://sourceforge.net/p/healpix/bugs/51">51</A>)
</LI>
<LI>workaround for possible bug in Intel <TT>icc</TT> 14.0 compiler
</LI>
<LI>bug fix in Mollweide projection in <TT>map2tga</TT> when not looking at (0,0)
</LI>
<LI><TT>autotools</TT> updates
</LI>
<LI>deprecation warnings in <TT>alice2</TT>, soon to be replaced
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>

<P>

<h4><A NAME="SECTION_320_14000000000000000">
Fortran 90 facilities and
	subroutines</A>
</h4>

<UL>
<LI>HEALPix-F90 routines and facilities can now also be compiled with
the free Fortran95 compiler <SPAN  CLASS="textbf">g95</SPAN>
(<A NAME="tex2html_320_6"
  HREF="http://www.g95.org/">http://www.g95.org/</A>).
</LI>
<LI>A separate <TT>build</TT> directory is used to store the objects,
modules, ... produced during the compilation of the source codes
</LI>
<LI>improved handling of long FITS keywords, now producing FITS files
fully compatible with the
<A NAME="tex2html_320_7"
  HREF="http://www.stsci.edu/institute/software_hardware/pyfits"><TT>PyFITS</TT></A>
and 
<TT>Astropy</TT> (<A NAME="tex2html_320_8"
  HREF="http://www.astropy.org/">www.astropy.org</A>)
<TT>Python</TT> libraries
</LI>
<LI>improved FITS file parsing in 
<TT>generate_beam</TT>,
affecting the external <SPAN CLASS="MATH"><I>B</I>(<I>l</I>)</SPAN> reading in the F90 facilities 
<TT>alteralm</TT>, 
<TT>synfast</TT>, 
<TT>sky_ng_sim</TT>, 
<TT>smoothing</TT>.
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>

<P>

<h4><A NAME="SECTION_320_15000000000000000">
IDL</A>
</h4>

<UL>
<LI>addition of <TT>ialteralm</TT> to modify
  Spherical Harmonics coefficients (<SPAN CLASS="MATH"><I>a</I><SUB><I>lm</I></SUB></SPAN>).
</LI>
<LI>addition of <TT>planck_colors</TT> to
  modify current color table to one used in Planck 2013 publications.
</LI>
<LI><TT>cartview, gnomview, mollview, orthview</TT>:

<UL>
<LI>addition of  
    <TT>BAD_COLOR</TT>,
    <TT>BG_COLOR</TT> and
    <TT>FG_COLOR</TT> keywords to change the color of the
    missing pixels, background and foreground labels and lines.
</LI>
<LI>support for 
    <TT>COLT='planck1'</TT> and 
    <TT>COLT='planck2'</TT> to use the Planck color tables
    defined in <TT>planck_colors</TT>
  
</LI>
</UL>
</LI>
<LI>Bugs correction in 
<TT>bin_llcl</TT>,
<TT>query_disc</TT>.
</LI>
<LI>update of the required
    <A NAME="tex2html_320_9"
  HREF="http://idlastro.gsfc.nasa.gov/homepage.html"><TT>IDL-astron</TT> library</A>
routines, and their supporting <A NAME="tex2html_320_10"
  HREF="http://www.idlcoyote.com"><TT>Coyote</TT></A>
routines (2014-11-10).
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>

<P>

<h4><A NAME="SECTION_320_16000000000000000">
Java</A>
</h4>

<UL>
<LI>explicit deprecation warnings in the source codes
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>

<P>

<h4><A NAME="SECTION_320_17000000000000000">
Python</A>
</h4>

<UL>
<LI>switch to <A NAME="tex2html_320_11"
  HREF="https://github.com/healpy/healpy/releases"><TT>healpy
1.8.1</TT></A>
<UL>
<LI>fixes bugs in monopole removal, 
</LI>
<LI>adds orthographic projection,
</LI>
<LI>easier install on MacOSX
	
</LI>
</UL>
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>


<!-- ************************************************* -->



<h3><A NAME="SECTION_311_00010000000000000000">
New Features in HEALPix 3.11</A>
</h3>

<h4><A NAME="SECTION_311_00011000000000000000">
General</A>
</h4>

<UL>
<LI><A NAME="tex2html1"
  HREF="http://sourceforge.net/projects/libsharp"><TT>libsharp</TT></A>
C library used for Spherical Harmonics Transforms 
in Fortran and C++ since HEALPix 3.10
can now be compiled with <EM>any</EM> <TT>gcc</TT> version.
</LI>
</UL>
<A HREF="#top">    Back to Top</A><br><br>

<h4><A NAME="SECTION_311_00012000000000000000">
C++</A>
</h4>

<UL>
<LI>See General section above
</LI>
</UL>
<A HREF="#top">    Back to Top</A><br><br>

<h4><A NAME="SECTION_311_00013000000000000000">
Fortran 90 facilities and
	subroutines</A>
</h4>

<UL>
<LI>bug correction in <TT>query_disc</TT> 
	routine in <TT>inclusive</TT> mode
</LI>
<LI>bug correction in <TT>alm2map_spin</TT> 
	routine, which had its <TT>spin</TT> value set to 2
</LI>
<LI>See General section above
</LI>
</UL>
<A HREF="#top">    Back to Top</A><br><br>

<h4><A NAME="SECTION_311_00014000000000000000">
IDL</A>
</h4>

<UL>
<LI><TT>ang2pix_ring</TT> and
		<TT>pix2ang_nest</TT>
		routines now accept scalar arguments
</LI>
</UL>
<A HREF="#top">    Back to Top</A><br><br>

<!-- ************************************************* -->


<p>
For any problem, question or comment, see the <a href="support.php">Support</a> page.
</p>


<?php require('_footer.inc.php'); ?>
