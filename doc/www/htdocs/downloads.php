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
--></script>

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
HEALPix package can be downloaded 	
 from the <a href="http://sourceforge.net/projects/healpix/">SourceForge Download page</a> under the GNU General Public License, version2 <a href="http://www.gnu.org/licenses/gpl-2.0.html">(GPLv2)</a>.
</p>

<p>
Use of the HEALPix software package should be explicitly acknowledged in all publications in the following form:
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
<LI><A NAME="tex2html11"
  HREF="#SECTION_310_10000000000000000">Bug corrections and Improvements in Version 3.10</A>
<UL>
<!-- <LI><A NAME="tex2html12" HREF="#SECTION_310_11000000000000000">General</A> -->
<LI><A NAME="tex2html13"
  HREF="#SECTION_310_12000000000000000">C</A>
<LI><A NAME="tex2html14"
  HREF="#SECTION_310_13000000000000000">C++</A>
<LI><A NAME="tex2html15"
  HREF="#SECTION_310_14000000000000000">Fortran</A>
<LI><A NAME="tex2html16"
  HREF="#SECTION_310_15000000000000000">IDL</A>
<!-- <LI><A NAME="tex2html17" HREF="#SECTION_310_16000000000000000">Java</A> -->
<LI><A NAME="tex2html18"
  HREF="#SECTION_310_17000000000000000">Python</A>
</UL>

<LI><A NAME="tex2html11"
  HREF="#SECTION_300_10000000000000000">Bug corrections and Improvements in Version 3.00</A>
<UL>
<li><A name="tex2html3"
  href="#SECTION_300_11000000000000000">General</A>
<li><A name="tex2html4"
  href="#SECTION_300_12000000000000000">C</A>
<li><A name="tex2html5"
  href="#SECTION_300_13000000000000000">C++</A>
<li><A name="tex2html6"
  href="#SECTION_300_14000000000000000">Fortran</A>
<li><A name="tex2html7"
  href="#SECTION_300_15000000000000000">IDL</A>
<li><A name="tex2html8"
  href="#SECTION_300_16000000000000000">Java</A>
<li><A name="tex2html9"
  href="#SECTION_300_17000000000000000">Python</A>
</ul>
</ul>
<!--End of Table of Child-Links-->
</p>

<h3><a name="SECTION_310_10000000000000000">New Features in HEALPix 3.10</a></h3>
<!--
<h3><A NAME="SECTION_310_11000000000000000">
General</A>
</H3>
N/A
-->


<H3><A NAME="SECTION_310_12000000000000000">
C</A>
</H3>

<UL>
<LI>experimental GNU autotools support (undocumented); the standard
configuration script remains available
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>

<P>

<H3><A NAME="SECTION_310_13000000000000000">
C++</A>
</H3>

<UL>
<LI>Spherical Harmonics Transform library <TT>libpsht</TT> replaced by <A NAME="tex2html2"
  HREF="http://sourceforge.net/projects/libsharp"><TT>libsharp</TT></A>
(<A NAME="tex2html3"
  HREF="http://arxiv.org/abs/1303.4945">Reinecke &amp; Seljebotn, 2013</A>). 
<BR><EM>Note that 
some <A NAME="tex2html1"
  HREF="http://gcc.gnu.org/releases.html"><TT>gcc</TT> versions</A>
(4.4.1 to 4.4.6) crash with an internal compiler error during compilation of <TT>libsharp</TT></EM>. 
The problem has been fixed in <TT>gcc</TT> 4.4.7, 4.5.*, 4.6.*, 4.7.* and
newer versions and was not present in versions 4.2.* and 4.3.*.
</LI>
<LI>added <TT>boundaries()</TT> method to <TT>T_Healpix_Base</TT>
</LI>
<LI>experimental GNU autotools support (undocumented); the standard
configuration script remains available
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>

<P>

<H3><A NAME="SECTION_310_14000000000000000">
Fortran 90 facilities and
	subroutines</A>
</H3>

<UL>
<LI>all Fortran facilities now support most of <TT>cfitsio</TT>s ``<A NAME="tex2html4"
  HREF="http://heasarc.gsfc.nasa.gov/docs/software/fitsio/filters.html">Extended File
Name Syntax</A>'' features,
allowing the reading and processing of an arbitrary HDU and table column out of
remote, compressed FITS files. For example, setting 
<BR><TT>infile = ftp://</TT><I>url/file.fits</I><TT>.gz[</TT><I>extn</I><TT>][col </TT><I>colname</I><TT>]</TT>  
<BR>
in <TT>anafast</TT>
will download the FITS file <I>file.fits.gz</I> from <I>url</I>, 
uncompress it, open the HDU (extension) featuring keyword <TT>EXTNAME=</TT><I>extn</I>, or the one with 1-based rank number <I>extn</I>, read the table column
with <TT>TTYPE*=</TT><I>colname</I> out of it and will analyze it.
<BR>
It is also possible to perform a remote <TT>anafast</TT> analysis of a 
<A NAME="tex2html5"
  HREF="http://www.sciops.esa.int/index.php?project=planck&amp;page=Planck_Legacy_Archive">Planck Legacy Archive (PLA)</A>
sky map named <I>map.fits</I> via the PLA <A NAME="tex2html6"
  HREF="http://pla.esac.esa.int/pla/aio/">AIO
Subsystem</A>
by simply setting
<TT>infile=http://pla.esac.esa.int/pla/aio/product-action?MAP.MAP_ID=</TT><I>map.fits</I>
as input map file.
</LI>
<LI>yet faster 
<TT>synfast</TT>,
<TT>anafast</TT>,
<TT>smoothing</TT> thanks to <A NAME="tex2html7"
  HREF="http://sourceforge.net/projects/libsharp"><TT>libsharp</TT></A>
routines (see warning on gcc releases above).
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>

<P>

<H3><A NAME="SECTION_310_15000000000000000">
IDL</A>
</H3>

<UL>
<LI>bug corrections: 
	<A HREF="#idl:query_disc"><TT>query_disc</TT></A>: correct handling of empty disc; 
	<A HREF="#idl:bin_llcl"><TT>bin_llcl</TT></A>: correct handling of optional argument.

<P>
</LI>
<LI>double precision of input now preserved in
	<A HREF="#idl:gaussbeam"><TT>gaussbeam</TT></A> and 
	<A HREF="#idl:euler_matrix_new"><TT>euler_matrix_new</TT></A>.

<P>
</LI>
<LI><TT>fits2cl</TT>: addition of 
	<TT>/PLANCK1</TT> keyword
	to read best fit <SPAN CLASS="MATH"><I>C</I>(<I>l</I>)</SPAN> model to Planck 2013 + external data.

<P>
</LI>
<LI>it is now possible to read a specific FITS file extension identified by its
	(0-based) number or its case-insensitive EXTNAME value with the <TT>Extension</TT>
	keyword added to 
	<A HREF="#idl:fits2cl"><TT>fits2cl</TT></A>,
	<A HREF="#idl:getsize_fits"><TT>getsize_fits</TT></A>,
	<A HREF="#idl:read_fits_map"><TT>read_fits_map</TT></A>,
	<A HREF="#idl:read_fits_s"><TT>read_fits_s</TT></A> and 
	<A HREF="#idl:read_tqu"><TT>read_tqu</TT></A>.

<P>
</LI>
<LI>update of the required
	<A NAME="tex2html8"
  HREF="http://idlastro.gsfc.nasa.gov/homepage.html"><TT>IDL-astron</TT> library</A>
routines, and their supporting <A NAME="tex2html9"
  HREF="http://www.idlcoyote.com"><TT>Coyote</TT></A>
routines (2013-02-08).
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>

<P>

<!--
<H3><A NAME="SECTION_310_16000000000000000">
Java</A>
</H3>
N/A
<A HREF=#top>    Back to Top</A><br><br>
-->

<H3><A NAME="SECTION_310_17000000000000000">
Python</A>
</H3>
<ul><li>
	switch to <TT>healpy 1.5.0</TT>: addition of 
<A NAME="tex2html10"
  HREF="https://healpy.readthedocs.org/en/1.5.0/generated/healpy.sphtfunc.gauss_beam.html"><TT>gauss_beam</TT></A>
to generate Gaussian beam window function.
</li></ul>

<A HREF=#top>    Back to Top</A><br><br>

<!-- ******************************************************  -->

<br />
<HR>


<h3><a name="SECTION_300_10000000000000000">New Features in HEALPix 3.00</a></h3>
<P>
<!--
The previous version of the HEALPix package (3.00) offered the following new
features: -->


<h3><A name="SECTION_300_11000000000000000">
General</A>
</h3>
<ul><li>
Introduction of the script <TT>healpix_doc</TT> for easy access to the HEALPix
PDF and HTML documentation.
</li></ul>
<A href=#top>    Back to Top</A><br><br>

<h3><A name="SECTION_300_12000000000000000">
C</A>
</h3>

<ul>
<li>Interface has remained unchanged, but the code has been replaced by a C port
of the relevant HEALPix C++ functions, resulting in significant speedups.
</li>
<li>Additional functions are provided which support Nside values up to <SPAN CLASS="MATH">2<SUP>29</SUP></SPAN>.
They have the same name as the traditional functions, with a ''64'' suffix appended.
</li>
</ul>
<A href=#top>    Back to Top</A><br><br>

<h3><A name="SECTION_300_13000000000000000">
C++</A>
</h3>

<ul>
<li>Query routines:
 query_polygon() and query_polygon_inclusive() added.
Query routines now return lists of pixel ranges instead of lists of pixels,
 which is much more economic.
Inclusive query routines: tradeoff between performance and number of false
 positives is tuneable.
Queries now work natively in both NESTED and RING schemes. Operations on
 the NESTED scheme are typically slower than in RING, but still much faster
 than computing the query in RING and converting all pixel numbers to NESTED
 afterwards.

</li>
<li>Healpix_Base:
Healpix_Base and Healpix_Base2 have been merged into the templated class
 T_Healpix_Base; functionality is still available under the old names.
Various performance improvements to T_Healpix_Base functionality

</li>
<li>User-friendliness:
module parameters can now optionally be passed on the command line instead
 of using a parameter file. For example:
<BR>   <TT>anafast_cxx nlmax=500 infile=test.fits iter_order=3 <...></TT> 
<br>
Facilities now check input maps for undefined pixels before calling map2alm().
 If undefined pixels are found, a warning is printed, and the pixels are set
 to zero. udgrade_cxx refuses downgrading of polarised maps (which would produce
 unphysical results)

</li>
<li>Bug fixes: accuracy of pix2ang near the poles at high resolutions has been improved.

</li>
<li>Configuration: optional autoconf support

</li>
<li>Interface changes:

<ul>
<li>Healpix_Base::query_*(): new interface
</li>
<li>cxxutils.h has been split up into
 announce.h (dealing with module banners), 
 share_utils.h (dealing with subdividing tasks between multiple workers) and
 string_utils.h (dealing with string manipulation and file parsing)
</li>
<li>psht.h: interface to alm_info changed in order to add MPI support
</li>
<li>ylmgen_c.h: Ylmgen_init() interface has changed
</li>
<li>bluestein.h: bluestein_i() interface changed
</li>
</ul>
</li>
</ul>
<A href=#top>    Back to Top</A><br><br>


<h3><A name="SECTION_300_14000000000000000">
Fortran 90 facilities and
subroutines
</A>
</h3>

<ul>
<li>Compressed and/or remote (ftp or http) FITS files can now be
read. <a href="http://heasarc.gsfc.nasa.gov/fitsio/">CFITSIO</a> 3.14 or more is now required;
</li>
<li>introduction of the 
<TT>process_mask</TT>
facility to compute the angular distance of valid
pixels to the closest invalid pixels for a input binary mask, and of the
supporting routines 
<TT>dist2holes_nest</TT>, 
<TT>fill_holes_nest</TT>, 
<TT>maskborder_nest</TT>,
size_holes_nest;
</li>
<li>the pixel query routine
 <TT>query_disc</TT>
has been improved and will return fewer
false positive pixels in the 
inclusive mode;
</li>
<li>improved accuracy of the co-latitude calculation in the vicinity
of the poles at high resolution in 
<TT>nest2ring, ring2nest, pix2ang_*, pix2vec_*, <SPAN CLASS="MATH">...</SPAN></TT>;
</li>
<li><TT>sky_ng_sim</TT> now allows the computation
of the spatial derivatives of the non Gaussian map being produced, and the
output of the <SPAN CLASS="MATH"><I>a</I><SUB><I>lm</I></SUB></SPAN> coefficients of that map;
</li>
<li><TT>anafast</TT> now allows the
pro/down-grading of the input mask to match the resolution of the map(s) being
analyzed;
</li>
<li>the median filter routine <TT>medfiltmap</TT>, used by the facility
<TT>median_filter</TT> is now parallelized.
</li>
</ul>
<A href=#top>    Back to Top</A><br><br>


<h3><A name="SECTION_300_15000000000000000">
IDL</A>
</h3>

<ul>
<li>New routines to go from circular beam profile to transfer function
(<TT>beam2bl</TT>), 
and back (<TT>bl2beam</TT>); 
to go from indexed list of <SPAN CLASS="MATH"><I>a</I><SUB><I>lm</I></SUB></SPAN> to a(l,m) 2D table
(<TT>alm_i2t</TT>), 
and back
(<TT>alm_t2i</TT>); and to compute the angular distance
between pairs of vectors (<TT>angulardistance</TT>).
</li>
<li>addition of <TT>iprocess_mask</TT>
interface to F90 <TT>process_mask</TT> facility to compute the angular distance of valid
pixels to the closest invalid pixels for a input binary mask.
</li>
<li>creation of <TT>hpx2dm</TT> routine to generate
DomeMaster images of HEALPix maps that can be projected on planetariums.
</li>
<li>the pixel query routines 
<TT>query_triangle</TT>, 
<TT>query_polygon</TT>, 
and in particular <TT>query_disc</TT>, 
have been improved and will return fewer
false positive pixels in the <EM>inclusive</EM> mode
</li>
<li>improved accuracy of the co-latitude calculation in the vicinity
of the poles at high resolution in 
<TT>nest2ring, ring2nest, pix2ang_*, pix2vec_*, <SPAN CLASS="MATH">...</SPAN></TT>
</li>
<li><TT>cartview, gnomview, mollview, orthview</TT>:
 the length and spacing of the headless vectors used to represent
polarization is now user-controlled  via 
<TT>POLARIZATION</TT>
keyword. The <TT>COLT</TT> keyword now
allows the use of an interactively modified color table.

</li>
<li><TT>orthview</TT> now accepts
<TT>STAGGER</TT> keyword to overplot staggered
spheres (with a twist) in order to detect periodic boundary conditions on the
sky
</li>
<li><TT>fits2cl</TT>: addition of <TT>WMAP7</TT> keyword
to read best fit <SPAN CLASS="MATH"><I>C</I>(<I>l</I>)</SPAN> model to WMAP 7yr data.
</li>
<li><TT>read_fits_map</TT> can now read
<SPAN CLASS="MATH"><I>Nside</I></SPAN>=8192 HEALPix maps and is generally faster than previously for smaller
maps
</li>
<li>update of <TT>astron</TT> library routines (01-Feb-2012).
</li>
</ul>
<A href=#top>    Back to Top</A><br><br>


<h3><A name="SECTION_300_16000000000000000">
Java</A>
</h3>

<ul>
<li>Core functionality has been reimplemented from scratch in the form of the
"healpix.essentials" package. It is strongly recommended to use this package
directly in future projects making use of Java HEALPix.
"healpix.essentials" is a port of the HEALPix C++ library and presents a very
similar interface.

The "healpix.core" package is still provided. It uses "healpix.essentials"
internally, and its interface has been kept stable as much as possible.
Some adaptations in user code will still be necessary, however.
Please note that using "healpix.core" will result in slightly lower performance
than calling "healpix.essentials" methods directly, because of the necessary
data conversion.
</li>
<li>New features and improvements introduced with the HealpixBase class, compared
to the HealpixIndex, Healpix and PixTools classes:

<ul>
<li>close similarities with Healpix_Base_T class from HEALPix C++, which allows
 simultaneous development and bug fixes for both.
</li>
<li>support for arbitrary positive Nside values in RING scheme; no longer limited
 to powers of 2
</li>
<li>maximum supported Nside value: <SPAN CLASS="MATH">2<SUP>29</SUP></SPAN>
</li>
<li>significant performance improvements: most methods have been accelerated
 by integral factors, some by more than an order of magnitude.
</li>
<li>re-implementation of queryDisc and queryPolygon, with same new features
as the C++ implementation (see above).
</li>
<li>the HealpixProc class offers a procedural (instead of object-oriented)
 interface to the HealpixBase functionality, which simplifies transition
 for users of the "Healpix" and "PixTools" classes.
 NOTE: this only works for Nside parameters which are powers of 2
</li>
<li>many bug fixes
</li>
<li>no external library dependencies, except for "nom.tam.fits" if FITS I/O is
 required
</li>
</ul>
</li>
</ul>
<A href=#top>    Back to Top</A><br><br>


<h3><A name="SECTION_300_17000000000000000">
Python</A>
</h3>

<ul>
<li>the 
<A name="tex2html1"
  href="https://github.com/healpy/healpy"><TT>healpy</TT></A>
package (C. Rosset, A. Zonca et al.) is now part of HEALPix
</li>
</ul>
<A href=#top>    Back to Top</A><br><br>

<p>
For any problem, question or comment, see the <a href="support.php">Support</a> page.
</p>


<?php require('_footer.inc.php'); ?>
