<?php require('_header.inc.php'); ?>




<h2>Downloading HEALPix</h2>

<p>
The HEALPix software can be downloaded freely, without registration.
However, if you wish to be kept informed of HEALPix developments, updates and new releases, provide your name and e-mail address below:
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
<p>		
<A NAME="top"></A>
HEALPix package can be downloaded 	
 from the <a href="http://sourceforge.net/projects/healpix/">SourceForge Download page</a>.
</p>

<p>
Use of the HEALPix software package should be explicitly acknowledged in all publications in the following form:
              <ul>
                <li>
                  an acknowledgment statement &ndash; "Some of the results in this paper have been derived using the HEALPix (<a href="http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=2005ApJ...622..759G&amp;db_key=AST&amp;high=41069202cf02947">K.M. G&oacute;rski et al., 2005, ApJ, 622, p759</a>) package", and
                </li><br>
                <li>
                  at the first use of the HEALPix acronym, a footnote placed in the main body of the paper referring to the HEALPix website &ndash; currently <a href="http://healpix.sf.net">http://healpix.sf.net</a>
                </li>
</ul>
</p>
<h2>New Features in HEALPix 3.00</h2>
<P>
<!--
The latest version of the HEALPix package (3.00) offers the following new
features: -->

<!--Table of Child-Links-->

<UL CLASS="ChildLinks">
<LI><A NAME="tex2html3"
  HREF="#SECTION00011000000000000000">General</A>
<LI><A NAME="tex2html4"
  HREF="#SECTION00012000000000000000">C</A>
<LI><A NAME="tex2html5"
  HREF="#SECTION00013000000000000000">C++</A>
<LI><A NAME="tex2html6"
  HREF="#SECTION00014000000000000000">Fortran</A>
<LI><A NAME="tex2html7"
  HREF="#SECTION00015000000000000000">IDL</A>
<LI><A NAME="tex2html8"
  HREF="#SECTION00016000000000000000">Java</A>
<LI><A NAME="tex2html9"
  HREF="#SECTION00017000000000000000">Python</A>
</UL>
<!--End of Table of Child-Links-->
</p>

<h3><A NAME="SECTION00011000000000000000">
General</A>
</h3>
<ul><li>
Introduction of the script <TT>healpix_doc</TT> for easy access to the HEALPix
PDF and HTML documentation.
</li></ul>
<A HREF=#top>    Back to Top</A><br><br>

<h3><A NAME="SECTION00012000000000000000">
C</A>
</h3>

<UL>
<LI>Interface has remained unchanged, but the code has been replaced by a C port
of the relevant HEALPix C++ functions, resulting in significant speedups.
</LI>
<LI>Additional functions are provided which support Nside values up to <SPAN CLASS="MATH">2<SUP>29</SUP></SPAN>.
They have the same name as the traditional functions, with a ''64'' suffix appended.
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>

<h3><A NAME="SECTION00013000000000000000">
C++</A>
</h3>

<UL>
<LI>Query routines:
 query_polygon() and query_polygon_inclusive() added.
Query routines now return lists of pixel ranges instead of lists of pixels,
 which is much more economic.
Inclusive query routines: tradeoff between performance and number of false
 positives is tuneable.
Queries now work natively in both NESTED and RING schemes. Operations on
 the NESTED scheme are typically slower than in RING, but still much faster
 than computing the query in RING and converting all pixel numbers to NESTED
 afterwards.

</LI>
<LI>Healpix_Base:
Healpix_Base and Healpix_Base2 have been merged into the templated class
 T_Healpix_Base; functionality is still available under the old names.
Various performance improvements to T_Healpix_Base functionality

</LI>
<LI>User-friendliness:
module parameters can now optionally be passed on the command line instead
 of using a parameter file. For example:
<BR>   <TT>anafast_cxx nlmax=500 infile=test.fits iter_order=3 <...></TT> 
<br>
Facilities now check input maps for undefined pixels before calling map2alm().
 If undefined pixels are found, a warning is printed, and the pixels are set
 to zero. udgrade_cxx refuses downgrading of polarised maps (which would produce
 unphysical results)

</LI>
<LI>Bug fixes: accuracy of pix2ang near the poles at high resolutions has been improved.

</LI>
<LI>Configuration: optional autoconf support

</LI>
<LI>Interface changes:

<UL>
<LI>Healpix_Base::query_*(): new interface
</LI>
<LI>cxxutils.h has been split up into
 announce.h (dealing with module banners), 
 share_utils.h (dealing with subdividing tasks between multiple workers) and
 string_utils.h (dealing with string manipulation and file parsing)
</LI>
<LI>psht.h: interface to alm_info changed in order to add MPI support
</LI>
<LI>ylmgen_c.h: Ylmgen_init() interface has changed
</LI>
<LI>bluestein.h: bluestein_i() interface changed
</LI>
</UL>
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>


<h3><A NAME="SECTION00014000000000000000">
Fortran 90 facilities and
subroutines
</A>
</h3>

<UL>
<LI>Compressed and/or remote (ftp or http) FITS files can now be
read. <a href="http://heasarc.gsfc.nasa.gov/fitsio/">CFITSIO</a> 3.14 or more is now required;
</LI>
<LI>introduction of the 
<TT>process_mask</TT>
facility to compute the angular distance of valid
pixels to the closest invalid pixels for a input binary mask, and of the
supporting routines 
<TT>dist2holes_nest</TT>, 
<TT>fill_holes_nest</TT>, 
<TT>maskborder_nest</TT>,
size_holes_nest;
</LI>
<LI>the pixel query routine
 <TT>query_disc</TT>
has been improved and will return fewer
false positive pixels in the 
inclusive mode;
</LI>
<LI>improved accuracy of the co-latitude calculation in the vicinity
of the poles at high resolution in 
<TT>nest2ring, ring2nest, pix2ang_*, pix2vec_*, <SPAN CLASS="MATH">...</SPAN></TT>;
</LI>
<LI><TT>sky_ng_sim</TT> now allows the computation
of the spatial derivatives of the non Gaussian map being produced, and the
output of the <SPAN CLASS="MATH"><I>a</I><SUB><I>lm</I></SUB></SPAN> coefficients of that map;
</LI>
<LI><TT>anafast</TT> now allows the
pro/down-grading of the input mask to match the resolution of the map(s) being
analyzed;
</LI>
<LI>the median filter routine <TT>medfiltmap</TT>, used by the facility
<TT>median_filter</TT> is now parallelized.
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>


<h3><A NAME="SECTION00015000000000000000">
IDL</A>
</h3>

<UL>
<LI>New routines to go from circular beam profile to transfer function
(<TT>beam2bl</TT>), 
and back (<TT>bl2beam</TT>); 
to go from indexed list of <SPAN CLASS="MATH"><I>a</I><SUB><I>lm</I></SUB></SPAN> to a(l,m) 2D table
(<TT>alm_i2t</TT>), 
and back
(<TT>alm_t2i</TT>); and to compute the angular distance
between pairs of vectors (<TT>angulardistance</TT>).
</LI>
<LI>addition of <TT>iprocess_mask</TT>
interface to F90 <TT>process_mask</TT> facility to compute the angular distance of valid
pixels to the closest invalid pixels for a input binary mask.
</LI>
<LI>creation of <TT>hpx2dm</TT> routine to generate
DomeMaster images of HEALPix maps that can be projected on planetariums.
</LI>
<LI>the pixel query routines 
<TT>query_triangle</TT>, 
<TT>query_polygon</TT>, 
and in particular <TT>query_disc</TT>, 
have been improved and will return fewer
false positive pixels in the <EM>inclusive</EM> mode
</LI>
<LI>improved accuracy of the co-latitude calculation in the vicinity
of the poles at high resolution in 
<TT>nest2ring, ring2nest, pix2ang_*, pix2vec_*, <SPAN CLASS="MATH">...</SPAN></TT>
</LI>
<LI><TT>cartview, gnomview, mollview, orthview</TT>:
 the length and spacing of the headless vectors used to represent
polarization is now user-controlled  via 
<TT>POLARIZATION</TT>
keyword. The <TT>COLT</TT> keyword now
allows the use of an interactively modified color table.

</LI>
<LI><TT>orthview</TT> now accepts
<TT>STAGGER</TT> keyword to overplot staggered
spheres (with a twist) in order to detect periodic boundary conditions on the
sky
</LI>
<LI><TT>fits2cl</TT>: addition of <TT>WMAP7</TT> keyword
to read best fit <SPAN CLASS="MATH"><I>C</I>(<I>l</I>)</SPAN> model to WMAP 7yr data.
</LI>
<LI><TT>read_fits_map</TT> can now read
<SPAN CLASS="MATH"><I>Nside</I></SPAN>=8192 HEALPix maps and is generally faster than previously for smaller
maps
</LI>
<LI>update of <TT>astron</TT> library routines (01-Feb-2012).
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>


<h3><A NAME="SECTION00016000000000000000">
Java</A>
</h3>

<UL>
<LI>Core functionality has been reimplemented from scratch in the form of the
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
</LI>
<LI>New features and improvements introduced with the HealpixBase class, compared
to the HealpixIndex, Healpix and PixTools classes:

<UL>
<LI>close similarities with Healpix_Base_T class from HEALPix C++, which allows
 simultaneous development and bug fixes for both.
</LI>
<LI>support for arbitrary positive Nside values in RING scheme; no longer limited
 to powers of 2
</LI>
<LI>maximum supported Nside value: <SPAN CLASS="MATH">2<SUP>29</SUP></SPAN>
</LI>
<LI>significant performance improvements: most methods have been accelerated
 by integral factors, some by more than an order of magnitude.
</LI>
<LI>re-implementation of queryDisc and queryPolygon, with same new features
as the C++ implementation (see install:cpp:queryabove).
</LI>
<LI>the HealpixProc class offers a procedural (instead of object-oriented)
 interface to the HealpixBase functionality, which simplifies transition
 for users of the "Healpix" and "PixTools" classes.
 NOTE: this only works for Nside parameters which are powers of 2
</LI>
<LI>many bug fixes
</LI>
<LI>no external library dependencies, except for "nom.tam.fits" if FITS I/O is
 required
</LI>
</UL>
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>


<h3><A NAME="SECTION00017000000000000000">
Python</A>
</h3>

<UL>
<LI>the 
<A NAME="tex2html1"
  HREF="https://github.com/healpy/healpy"><TT>healpy</TT></A>
package (C. Rosset, A. Zonca et al.) is now part of HEALPix
</LI>
</UL>
<A HREF=#top>    Back to Top</A><br><br>

<p>
For any problem, question or comment, see the <a href="support.php">Support</a> page.
</p>


<?php require('_footer.inc.php'); ?>
