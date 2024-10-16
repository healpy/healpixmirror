<?php require('_header.inc.php'); ?>


<!-- ********************** -->
<h2>Data in HEALPix format</h2>
<!-- ********************** -->
<br>
<ul>
<li>
The <a href="https://www.cosmos.esa.int/web/planck">Planck</a> satellite full sky maps, 
<a href="https://www.cosmos.esa.int/web/planck/publications">observed at 30 to 857 GHz with unprecedented accuracy</a>, 
and their decomposition in cosmological and astrophysical components are available in HEALPix format from ESA 
<a href="https://www.cosmos.esa.int/web/planck/pla">Planck Legacy Archive</a>, 
and NASA <a href="https://irsa.ipac.caltech.edu/Missions/planck.html">Infrared Science Archive</a>
</li>

<li>
The 
<a href="https://lambda.gsfc.nasa.gov/product/map/current/m_products.cfm" >Wilkinson 
    Microwave Anisotropy Probe (WMAP)</a> satellite data, see
    <a href="https://map.gsfc.nasa.gov/" >WMAP Technical Information</a>
</li>

<li>
Unofficial low-resolution <a href="https://faun.rc.fas.harvard.edu/ameisner/wssa/healpix.html">WISE 12 micron full-sky dust maps</a>
</li>

<li>
SDSS <a href="http://risa.stanford.edu/redmapper/">DR8 Depth maps</a>
</li>

<li>
The <a href="https://lambda.gsfc.nasa.gov/product/cobe/" >COBE</a>
data, including DMR and FIRAS
</li>

<li>
The Extended Sky Emission Repository of <a href="http://cade.irap.omp.eu/dokuwiki/doku.php?id=welcome">CADE</a>
which includes <a href="http://cade.irap.omp.eu/dokuwiki/doku.php?id=cobe">COBE/DIRBE</a> and many other surveys
</li>

<li>
The radio and infrared <a href="https://lambda.gsfc.nasa.gov/product/foreground/f_products.cfm">foregrounds</a> maps
including CO, Dust, Synchrotron, H-alpha and HI, as well as
<a href="https://lambda.gsfc.nasa.gov/product/iras/">IRAS</a> maps
</li>
<!--
<li>
The <a href="http://www.drao-ofr.hia-iha.nrc-cnrc.gc.ca/26msurvey/data.html"
    >DRAO</a> 1.4 GHz polarized maps
</li>
-->

<li>
The Schlegel, Finkbeiner, and Davis
    (<a href="https://arxiv.org/abs/astro-ph/9710327">astro-ph/9710327</a>,
 <a href="https://ui.adsabs.harvard.edu/abs/1998ApJ...500..525S/abstract">ApJ. 500, p.525</a>),
    combined
    <!--a href="http://astro.berkeley.edu/davis/dust/index.html"-->
    <a href="https://irsa.ipac.caltech.edu/applications/DUST/docs/background.html">COBE/DIRBE-IRAS/SISSA dust emission map</a>
</li>

<li>
An extensive list of 
<a href="https://lambda.gsfc.nasa.gov/product/expt/" >
    past, current and future CMB or SZ experiments and data availability</a> on Lambda web site
</li>

</ul>

<!-- ********************** -->
<h2>Applications related to HEALPix</h2>
<!-- ********************** -->

<p>

<ul>
<!--   ******Data simulation******  -->
<li>
The 
<a href="https://lambda.gsfc.nasa.gov/toolbox/tb_cmbfast_ov.cfm" >
    CMBFAST</a>, <a href="https://camb.info/">CAMB</a> and
<!-- a href="http://lesgourg.web.cern.ch/lesgourg/class.php">CLASS</a -->
<a href="http://class-code.net/">CLASS</a>
softwares can be used for the computation of the
theoretical spectra of CMB anisotropy. The HEALPix synfast
program reads in the output of these applications to allow one to
generate random realizations of the observable CMB sky
</li>

<li>
The 
<a href="https://cosmologist.info/lenspix" >
    LensPix</a>
software can be used to simulate lensed polarized CMB maps in HEALPix format using MPI
</li>

<li>
The <a href="https://www.apc.univ-paris7.fr/~delabrou/PSM/psm.html">Planck Sky Model (PSM)</a> 
and <a href="https://github.com/bthorne93/PySM_public">Python Sky Model (PySM)</a>
to simulate the IR and submm sky
</li>

<li>
Generation of CMB data streams and maps: <a href="https://sourceforge.net/projects/planck-ls/">
Planck LevelS pipeline</a> and <a href="https://github.com/hpc4cmb/toast">TOAST</a>
</li>

<li>
<a href="http://www.astro.iag.usp.br/~flask">FLASK</a>: Generation of lognormal or Gaussian correlated fields on the sphere
</li>

<li>
<a href="https://galprop.stanford.edu/index.php">GALPROP</a>: 
Numerical code for calculating the propagation of relativistic charged particles 
and the diffuse emissions they produce (now compatible with HEALPix 3.11 and newer)
</li>

<li>
<a href="https://lpsc.in2p3.fr/clumpy/intro.html">CLUMPY</a>: 
Computation of photon fluxes from dark matter annihilation or decay in galaxy haloes
</li>

<li>
<a href="https://mocks.cita.utoronto.ca/index.php/WebSky_Extragalactic_CMB_Mocks">WebSky
Extragalactic CMB Mocks</a>:
Consistent simulations of CMB lensing, (thermal and kinetic) SZ and CIB
available as HEALPix FITS files
</li>


<!--   ******Visualization******  -->
<li style="margin-top:1.5em">
Visualization tools supporting HEALPix maps in FITS format:
<a href="https://aladin.u-strasbg.fr/">Aladin&nbsp;Sky&nbsp;Atlas</a> 
(which can overlay any astronomical map or catalog)
<!-- a href="http://helios.av.it.pt/projects/healpixviewer">HealpixViewer</a>,
<a href="http://www.ast.cam.ac.uk/~vlad/univiewer/index.html">Univiewer</a -->
and
<a href="https://lambda.gsfc.nasa.gov/toolbox/tb_skyviewer_ov.cfm">SkyViewer</a>
</li>

<li>
HEALPix based <a href="https://aladin.u-strasbg.fr/hips/">Hierarchical Progressive Surveys (HiPS)</a> mechanism
to access, visualize and browse images, catalogues and cube data
</li>

<li>
<a href="https://sky.esa.int">ESA Sky</a> to search, retrieve, explore and visualize the entire sky as observed by ESA (and other) astronomy missions (<a href="https://www.cosmos.esa.int/web/esdc/esasky-help">help desk</a>)
</li>

<li>
<a href="http://montage.ipac.caltech.edu/">Montage Astronomical Image Mosaic Engine</a>: to stitch and reproject astronomical images, including <a href="http://montage.ipac.caltech.edu/docs/HEALPix/">those in HEALPix format</a>
<!-- https://en.wikipedia.org/wiki/Montage_Image_Mosaic_Software -->
</li>

<!--   ******Data analysis******  -->
<li  style="margin-top:1.5em">
<a href="https://github.com/healpy/healpy">Healpy</a>: 
python wrapper to HEALPix (now included in HEALPix package)
</li>

<li>
<a href="https://mhealpy.readthedocs.io">mhealpy</a>: 
a python wrapper around Healpy for multi-resolution datasets
</li>

<li>
<a href="https://www.apc.univ-paris7.fr/APC_CS/Recherche/Adamis/MIDAS09/software/s2hat/s2hat.html">S2Hat</a>:
Scalable Spherical Harmonics transforms
</li>

<li>
<a href="http://www2.iap.fr/users/hivon/software/PolSpice">PolSPICE</a>: 
Temperature+Polarization angular power spectrum extraction tool dealing with the effects of
cut-sky, beam smoothing, noise contamination...
</li>

<li>
<a href="https://github.com/damonge/NaMaster">NaMaster</a>:
Power spectrum extraction using MASTER approach, on curved sky and flat sky.
</li>

<!-- commented out 2016-09-13
<li>
<a href="http://lcdm.astro.illinois.edu/code/aps.html">APS</a>:
Quadratic estimator of the galaxy angular power spectrum
</li>
-->

<li>
<a href="http://www2.iap.fr/users/sousbie/web/html/indexd41d.html?"> DisPerSE</a>: 
Automatic identification of persistent structures in 2D & 3D
</li>

<li>
Wavelets on the Sphere: 
<a href="http://www.cosmostat.org//isap.html">iSAP</a>, 
    <a href="http://irfu.cea.fr/en/Phocea/Vie_des_labos/Ast/ast_visu.php?id_ast=895">MRS</a>,
<!--  a href="http://www.s2let.org/">S2LET</a  --> 
    <a href="https://astro-informatics.github.io/s2let/">S2LET</a>, 
<!--    <a href="https://gitorious.org/spherelib">spherelib (previously SphereLab)</a>           -->
<!--    <a href="https://forge.in2p3.fr/projects/sphere-lib">spherelib (previously SphereLab)</a>-->
    <a href="https://gitlab.in2p3.fr/spherelib/spherelib">spherelib (previously SphereLab)</a>
</li>

<li>
<a href="http://www2.iap.fr/users/lavaux/software/flints.html">FLINTS</a>: 
interpolation algorithm of fields sampled on a sphere
</li>

<!-- commented out 2016-09-13
<li>
Take a look at a very useful compilation of 
<a href="http://www.mpa-garching.mpg.de/~banday/CMB.html" >
    CMB Resources</a>
by Anthony J. Banday
</li>
-->

<!--   ******ASCL******  -->
<li  style="margin-top:1.5em">
<a href="https://ascl.net/code/search/healpix">Codes related to HEALPix</a> (<a href="https://ascl.net/1107.018">ascl:1107.018</a>) in 
Astrophysics Source Code Library.
</li>
</ul>

<!-- ********************** -->
<h2>HEALPix in the literature</h2>
<!-- ********************** -->

<p>
Published articles using HEALPix: 
<ul>
<li>
First HEALPix paper: <a href="https://ui.adsabs.harvard.edu/abs/2005ApJ...622..759G/abstract">K.M. G&oacute;rski et al., 2005, ApJ, 622, p759</a><br>(among the 10 <a href="http://cococubed.asu.edu/journal_pages/top50.shtml">most cited astronomical articles</a> published in 2005)
</li>
<li>HEALPix 
on 

<!-- a href="http://inspirebeta.net/search?ln=en&p=FIND+C+ASTRO-PH%2F9812350+or+refersto%3Arecid%3A659804+or+fulltext%3Ahealpix&f=&action_search=Search&sf=&so=d&rm=&rg=25&sc=0&of=hb">INSPIRE (High Energy Physics &amp; Cosmology)</a>,<br -->
<a href="https://inspirehep.net/search?ln=en&p=FIND+C+ASTRO-PH%2F9812350+or+refersto%3Arecid%3A659804+or+fulltext%3Ahealpix&f=&action_search=Search&sf=&so=d&rm=&rg=25&sc=0&of=hb">INSPIRE (High Energy Physics &amp; Cosmology)</a>,
<!-- br>
<a href="http://adsabs.harvard.edu/cgi-bin/nph-ref_query?bibcode=2005ApJ...622..759G&amp;refs=CITATIONS&amp;db_key=AST">ADS Classic</a>
&amp; -->

<a href="https://ui.adsabs.harvard.edu/#search/q=full%3A%28healpix+OR+healpy%29&sort=date+desc">ADS&nbsp;(Astrophysics)</a>,

<!-- ADS 2.0 now deprecated
&amp;
<a href="http://adslabs.org/adsabs/search/?q=full%3A%22healpix%22&month_from=&year_from=&month_to=&year_to=&db_f=&nr=&bigquery=&re_sort_type=DATE&re_sort_dir=desc">ADS 2.0</a> 
-->

<!-- br -->

<a href="http://tinyurl.com/hyky2hy">Scopus</a> (requires an account)
<!-- http://preview.tinyurl.com/hyky2hy -->

and

<a href="https://scholar.google.com/scholar?scisbd=2&q=healpix&hl=en&as_sdt=0,5">Google Scholar</a>

 (Multidisciplinary)

</li>
</ul>

<p>
Misc
<ul>
<li>
<a href="https://en.wikipedia.org/wiki/HEALPix">Wikipedia Page on HEALPix</a>
</li>
<li>
	HEALPix on <a href="https://www.openhub.net/p/healpix">BlackDuck (previously Ohloh)</a> 
<!-- commented out 2018-08-30   and <a href="http://www.digplanet.com/wiki/HEALPix#">digplanet</a>  -->
</li>
<li>
<!-- a href="https://www.google.com/search?q=healpix&pws=0&biw=1395&bih=710&tbm=shop&source=lnms&sa=X&ved=0ahUKEwixwtO39-_JAhXHOBoKHQ0_CFgQ_AUICCgC&dpr=2#q=healpix&pws=0&tbm=bks&start=0">Some books mentioning HEALPix according to Google</a> (and some not)-->
Some 
<a href="https://www.google.com/search?q=healpix&tbm=nws">news</a>,
<a href="https://www.google.com/search?q=healpix&tbm=isch">images</a>, 
<a href="https://www.google.com/search?q=healpix&tbm=vid">videos</a>, 
<a href="https://www.google.com/search?q=healpix&tbm=bks">books</a>, 
 related to HEALPix according to Google
</li>
</ul>




<?php require('_footer.inc.php'); ?>
