<?php require('_header.inc.php'); ?>


<h2>Introduction</h2>

<p>
HEALPix is an acronym for 
<strong>H</strong>ierarchical 
<strong>E</strong>qual 
<strong>A</strong>rea 
iso<strong>L</strong>atitude 
<strong>Pix</strong>elization 
of a sphere. As suggested in the name, this pixelization produces a subdivision of a spherical surface in which each pixel covers the same surface area as every other pixel.
</p>

<h2>Purpose</h2>

<p>
              The original motivation for devising HEALPix was one of necessity.
              Satellite missions to measure the cosmic microwave background (CMB)
              anisotropy -- NASA's 

              <a href="http://map.gsfc.nasa.gov/" target="_top">Wilkinson Microwave
              Anisotropy Probe (WMAP)</a>,

              and currently 
              operating ESA's mission

              <a href="http://www.rssd.esa.int/index.php?project=Planck" target="_top">Planck</a>

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

                  <a href="http://space.gsfc.nasa.gov/astro/cobe/skymap_info.html"
                     target="_top">
                    Quadrilateralized Spherical Cube
                  </a>

                  used for the NASA's

                  <a href="http://space.gsfc.nasa.gov/astro/cobe" target="_top">COBE</a>

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
<p>
              The HEALPix package 
              contains a suite of programs which allow all of the following (and more):
              <br />
              <ul>
                <li>
                  Programs for fast simulation and analysis of full-sky maps of CMB
                  temperature and polarization anisotropy
                  (<a href="snapshots.php" target="_top">sky maps preview</a>)
                  up to sub-arcminute angular resolution.
<!--(your computer's RAM permitting, see <a href="pdf/benchmark1.pdf">benchmarks</a>).-->
                </li>
                <!-- br -->
                <li>
                  C, C++, Fortan 90, IDL, Java, Python implementation of most 
		  routines and facilities (for third party implementation in other languages, see <a href="http://sourceforge.net/p/healpix/wiki/Available%20languages/">this Healpix wiki page</a>)
                </li>
                <!-- br -->
                <li>
                  Most critical routines are parallelised
                </li>
                <!-- br -->
                <li>
                  Optimised <SPAN CLASS="MATH"><I>P</I><SUB><I>lm</I></SUB></SPAN> C library for better performance.
                </li>
                <!-- br -->
                <li>
                  Routines to read and write the
                  <a href="http://fits.gsfc.nasa.gov/" target="_top">FITS</a>
                  formatted map data sets,
                  and allowing the IDL, Java or Python display of your results.
                </li>
                <!-- br -->
                <li>
                  Fortran 90 and C++ visualization facilities.
                </li>
                <!-- br -->
                <li>
                  <a href="documentations.php">Comprehensive documentation</a> 
		(<a href="doc/pdf_index.pdf">PDF</a> and 
		<a href="doc/main.htm">HTML</a>)
                </li>
                <!-- br -->
                <li>
                  Automated installation and build scripts.
                </li>
                <!-- br -->
                <li>
                  Programs to search the maps for pixel neighbours and extrema 
		  of a random field.
                </li>
                <!-- br -->
                <li>
                  Pixel queries in discs, triangles, polygons and strips
                </li>
                <!-- br -->
                <li>
                  Programs to manage, modify and rotate spherical harmonic 
		  coefficients of arbritrary maps	
                </li>
                <!-- br -->
                <li>
                  Programs to perform median filtering of sky maps
                </li>
                <!-- br -->
                <li>
                  Constrained and non-Gaussian realization facilities.
                </li>
                <!-- br -->
                <li>
                  Mask processing facilities.
                </li>
                <!-- br -->
                <li>
                  An IDL toolkit for pixel manipulation and
                  <a href="http://fits.gsfc.nasa.gov/" target="_top">FITS</a>
                  file manipulation.
                </li>
                <!-- br -->
              </ul>
</p>
<p>
    For a detailed list of the new features added in the latest Healpix release, see
    the <a href="downloads.php">Getting HEALPix page</a>.
</p>

<?php require('_footer.inc.php'); ?>
