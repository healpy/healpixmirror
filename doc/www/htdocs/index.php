<?php require('_header.inc.php'); ?>


<h2>Introduction</h2>

<p>
HEALPix is an acronym for 
<strong>H</strong>ierarchical 
<strong>E</strong>qual 
<strong>A</strong>rea 
iso<strong>L</strong>atitude 
<strong>Pix</strong>elation 
of a sphere. As suggested in the name, this pixelation produces a subdivision of a spherical surface in which each pixel covers the same surface area as every other pixel.
</p>

<h2>Purpose</h2>

<p>
              The original motivation for devising HEALPix was one of necessity.
              Satellite missions to measure the cosmic microwave background (CMB)
              anisotropy -- NASA&#039;s 

              <a href="http://map.gsfc.nasa.gov/">Wilkinson Microwave
              Anisotropy Probe (WMAP)</a>,

              and currently 
              operating ESA&#039;s mission

              <a href="http://www.cosmos.esa.int/web/planck">Planck</a>

              -- have been producing multi-frequency data
              sets sufficient for the construction of full-sky maps of the
              microwave sky at an angular resolution of a few arcminutes. The
              principal requirements in the development of HEALPix were to
              create a mathematical structure which supports a suitable
              discretization of functions on a sphere at sufficiently high
              resolution, and to facilitate fast and accurate statistical and
              astrophysical analysis of massive full-sky data sets.
              <br />
              <br />
              HEALPix satisfies these requirements because it possesses the
              following three essential properties:
              <br />
              <ol>
                <li>
                  The sphere is hierarchically tessellated into curvilinear
                  quadrilaterals. The lowest resolution partition is comprised
                  of 12 base pixels. Resolution of the tessellation increases
                  by division of each pixel into four new ones. 
		   The figure
                  below illustrates (clockwise from upper-left to bottom-left)
                  the resolution increase by three steps from the base level
                  (i.e., the sphere is partitioned, respectively, into 12, 48,
                  192, and 768 pixels). 
                </li>
                <li>
                  Areas of all pixels at a given resolution are identical.
                </li>
                <li>
                  Pixels are distributed on lines of constant latitude. This
                  property is essential for all harmonic analysis applications
                  involving spherical harmonics. Due to the iso-latitude
                  distribution of sampling points the speed of computation
                  of integrals over individual spherical harmonics scales as
                  ~N<sup>1/2</sup> with the total number of pixels, as opposed to the ~N
                  scaling for the non-iso-latitude sampling distributions
                  (examples of which are the

                  <a href="http://lambda.gsfc.nasa.gov/product/cobe/skymap_info_new.cfm">
                    Quadrilateralized Spherical Cube
                  </a>

                  used for the NASA&#039;s

                  <a href="http://lambda.gsfc.nasa.gov/product/cobe">COBE</a>

                  data, and any distribution based on the symmetries of the icosahedron).
                </li>
              </ol>
</p>

<p>
            <img src="images/gorski_f1.jpg" alt="HEALPix Grid Examples" width="513" height="510"><br />
</p>

<p> Applications of HEALPix to data processing and visualization have now spread well outside the original CMB field,
as can be seen from the <a href="resources.php">Resources</a> and <a href="gallery.php">Gallery</a> pages.
</p>

<h2>Main Features of HEALPix Software</h2>
<p>		The HEALPix software is available in C, C++, Fortran90, IDL, Java and Python.
		In each of these languages are available an extensive library of HEALPix specific tools 
		and their supporting routines as well as a suite of programs implementing the
		key features of HEALPix. Each package allows all of, or most of, the following (and more):
              	<br />
	      	<ul>
		<li>
		Spherical Harmonics Transforms:
              		<ul>	
                	<li>
                  	Fast simulation and analysis of full-sky maps of CMB
                  	temperature and polarization anisotropy
                  	(<a href="gallery.php">sky maps preview</a>)
                  	up to sub-arcminute angular resolution
			<!--(your computer&#039;s RAM permitting, see <a href="pdf/benchmark1.pdf">benchmarks</a>).-->
                	</li>
                	<!-- br -->
			<li>
			Filtering of sky maps with arbitrary circular window
			</li>
                	<!-- br -->
                	<li>
                  	Constrained and non-Gaussian realization facilities
                	</li>
                	<!-- br -->
                	<li>
     Highly optimised Spherical Harmonics Transforms library 
    (<a href="http://sourceforge.net/projects/libsharp"><tt>libsharp</tt></a>) 
     used by all implementations for better performance
                	</li>
                	<!-- br -->
                	<li>
                  	Forward and backward scalar and spin-weighted Spherical Harmonics Transforms
                	</li>
                	<!-- br -->
                	<li>
                  	Programs to manage, modify and rotate spherical harmonic 
		  	coefficients of arbitrary maps	
                	</li>
			</ul>
                </li><!-- br -->
		<li>
		Pixel manipulation:
			<ul>
			<li>
			Pixelation of the sphere supported down to a pixel size of 0.4&nbsp;mas (milli-arcseconds), corresponding to potentially 3.5&nbsp;10<sup>18</sup> pixels on the sphere
                	<!-- br -->
                	<li>
                  	Pixel queries in discs, triangles, polygons and strips
                	</li>
                	<!-- br -->
                	<li>
                  	Programs to search the maps for pixel neighbours and extrema 
		  	of a random field
                	</li>
                	<!-- br -->
                	<li>
                  	Median filtering of sky maps
                	</li>
			<!-- br -->
			<li>
			Mask processing facilities
			<!-- br -->
			</li>
			<li>
			Support for multi resolution maps (aka, Multi Order Coverage maps or MOC)
			<!-- br -->
			</li>
			</ul>
                </li><!-- br -->
		<li>
		General:
			<ul>
			<li>
                  	<a href="documentation.php">Comprehensive documentation</a> 
			(<a href="../doc/pdf/pdf_index.pdf"
                         onClick="_ga('send', 'event', 'PDF', 'Download', 'PDF Index');">PDF</a> and 
			<a href="../doc/html/main.htm">HTML</a>); 
			web-based and email <a href="support.php">support</a>
                	</li>
                	<!-- br -->
                	<li>
                  	Automated installation and build scripts
                	</li>
                	<!-- br -->
                	<li>
                  	Most critical routines are parallelized
                	<!-- br -->
			</ul>
			</li>
                </li><!-- br -->
		<li>
		Visualization:
			<ul>
                	<li>
                  	 Visualization facilities available in C++, Fortran 90, IDL, Java and Python
                	</li>
                	<!-- br -->
			<li>
                  	Facilities to output HEALPix maps into 
			<a href="http://earth.google.com/">Google Earth</a>/<a href="http://earth.google.com/sky/skyedu.html">Google Sky</a> compliant images
			and into <a href=" http://fulldome.ryanwyatt.net/fulldome_domemasterSpec_v05.pdf">DomeMaster</a> format used in planetariums.
                	</li>
                	<!-- br -->
              		</ul>
		</li><!-- br -->
		<li>
		Input/Output:
			<ul>
			<li>
			routines to manipulate and visualize the 
			<a href="http://fits.gsfc.nasa.gov/">FITS</a>
			files generally used for I/O
                	</li>
                	<!-- br -->
			</ul>
                </li><!-- br -->
		</ul>
</p>
<p>
    For a detailed list of the new features added in the latest HEALPix release, see
    the <a href="downloads.php">Getting HEALPix page</a>.<br>
    For third party implementations in other languages (including Matlab/Octave and Yorick), see <a href="http://sourceforge.net/p/healpix/wiki/Available%20languages/">this HEALPix wiki page</a>.<br>
    For third party extensions of HEALPix capabilities, see the <a href="resources.php">Resources page</a>.
</p>

<?php require('_footer.inc.php'); ?>
