<?php require('_header.inc.php'); ?>


<h2>HEALPix History</h2>

<p>
              HEALPix  was  originally devised  in early 1997 by 
		<u>Krzysztof M. G&oacute;rski</u> at the
              <a href="http://www.tac.dk/">(now defunct) Theoretical Astrophysics Center (TAC)</a>

              in Copenhagen, Denmark (he is currently at <a href="http://www.jpl.nasa.gov">Jet Propulsion Laboratory, JPL</a>).  
	<br>
	     Early development
              of the HEALPix concept and initial implementation was made in
              the spring of 1997 in collaborative work of K.M. G&oacute;rski
              with <u>Eric Hivon</u> (currently at the

              <!-- <a href="http://www.caltech.edu">California Institute of Technology</a>). -->
              <a href="http://www.iap.fr/?langue=en">Institut d&#039;Astrophysique 
									  de Paris, IAP</a>, France).
	<br>
              <u>Benjamin D. Wandelt</u>, then at the TAC (now at
	      <!-- 	
              <a href="http://illinois.edu/index.html">University of Illinois at Urbana-Champaign</a> 
		-->
              <a href="http://www.iap.fr/?langue=en">IAP</a>),

              has contributed critically to the further development of HEALPix
              and related mathematical methods. 
	<br>
		<u>Frode K. Hansen</u>, then at the
              TAC (now at

              <!-- <a href="http://www2.uniroma2.it/">Universita Roma II</a>), -->
              <a href="http://www.uio.no/english/">Universitetet i Oslo</a>, UIO),

              <u>Anthony J. Banday</u>
              (then at <a href="http://www.mpa-garching.mpg.de/mpa/index-en.shtml">Max Planck Institut f&#252;r Astrophysik (MPA)</a>,

              Garching, Germany, now at <a href="http://www.cesr.fr/?lang=en">Centre d&#039;Etude 
	       Spatiale du Rayonnement</a>, Toulouse, France) 

	     and <u>Matthias Bartelmann</u> (then at MPA, now at
 		
		<a href="http://www.ita.uni-heidelberg.de/index.shtml?lang=en"> Institut f&#252;r Theoretische Astrophysik</a>, 
 		Heidelberg, Germany)


		provided invaluable input required to make possible
              the first large release of the HEALPix software (Version 1.10).
              <br>
              <br>
              The following releases of the HEALPix software (Versions 1.20 to 3.30) and the current one (Version 3.31) benefited
              from:
		<ul>
		<li>
		the continuing dedicated effort of E. Hivon, 
		<u>Martin Reinecke</u> (<a href="http://www.mpa-garching.mpg.de/mpa/index-en.shtml">MPA</a>), and A. J. Banday, 
		the work of 

              <u>William O&#039;Mullane</u>
		(<a href="http://www.esa.int/About_Us/ESAC">European Space Astronomy Center, ESAC</a>), 

		<u>Hans Kristian Eriksen</u> 
		(<a href="http://www.uio.no/english">Universitetet i Oslo</a>),

		<u>Snorre Boasson</u> 
		(<a href="http://www.ntnu.edu/">NTNU</a>),

		<u>David Larson</u> 
		(<a href="http://www.jhu.edu/">Johns Hopkins University, JHU</a>),

		<u>Emmanuel Joliet</u> (<a href="http://www.ipac.caltech.edu/">IPAC</a>),

		<u>Paddy Leahy</u> (<a href="http://www.manchester.ac.uk">Manchester University</a>),

		<u>Graca Rocha</u> (JPL),
		</li>
		<li>
               the inclusion of 
		<u>Cyrille Rosset</u>&#039;s
		(<a href="http://www.apc.univ-paris7.fr/APC_CS/en">APC</a>),  
		<u>Andrea Zonca</u>&#039;s
		(<a href="http://www.ucsb.edu/">UCSB</a>), and
		<u>Leo Singer</u>&#039;s
		(<a href="http://www.caltech.edu">Caltech</a>)
		healpy package,
		</li>
		<li>
              and numerous contributions
              made by HEALPix users (see the

              <a href="#acknowledgments">Acknowledgments</a>).
		</li>
		</ul>


</p>
<a name="team"></a>
<h2>HEALPix Team</h2>
<p>
              The active support of HEALPix currently involves the following people (in alphabetical order):
   <!-- <table cellSpacing=0 cellPadding=0 border=0 width="525"><tr valign="top"><td width="525" valign="top"> -->
		<ul>
		<li>
	      A.J. Banday;
		</li>

		<li>
		K.M. G&oacute;rski;
		</li>

		<li>
                E. Hivon:
		IDL and F90 developments, release coordination, web site;
		</li>

		<li>
		E. Joliet:		Java developments;
		</li>


		<li>
              W. O&#039;Mullane:       Java developments;
		</li>

		<li>
              M. Reinecke:
		C, C++ and Java developments;
		</li>

		<!---
		<li>
		C. Rosset: Python developments;
		</li>
		--->

		<li>
		L. Singer: Python developments;
		</li>

		<li>
              A. Zonca: Python developments.
		</li>

   <!-- </td></tr></table>  -->


	</ul>
		<b>
	<p>
                This effort is not supported by any dedicated funds, and depends
                entirely on the voluntary collaboration of the mentioned individuals.
	</p>
              </b>

</p>


<!-- *************************************************************************************  -->


<A NAME="acknowledgments">
<h2>Acknowledgments</h2>
</A>

<p>
		We thank the following people for their contributions and/or suggestions, which
		significantly helped us to improve HEALPix throughout its several releases

            <table cellSpacing=0 cellPadding=0 border=0 width="601" class="tablelist">
              <tr valign="top">
                <td width="250" valign="top">
                    <ul>
                      <li>    Reza Ansari    			</li>
                      <li>    John Arballo    			</li>
                      <li>    Marc Ashdown   			</li>
                      <li>    Carlo Baccigalupi   		</li>
		      <li>    Karim 		Benabed   	</li> 
		      <li>    Aur&eacute;lien 	Benoit-L&eacute;vy   	</li> 
		      <li>    Jean-Philippe 	Bernard	  	</li>
                      <li>    Snorre Boasson   			</li>
		      <li>    Caroline 		Bot		</li>
                      <li>    Michela Botti   			</li>
		      <li>    Anthony 		Challinor 	</li>
                      <li>    St&eacute;phane Colombi   	</li>
		      <li>    Alain Coulais			</li>
                      <li>    Giancarlo de Gasperis   		</li>
		      <li>    Clive Dickinson     		</li>
                      <li>    Herv&eacute; Dole   		</li>
                      <li>    Olivier Dor&eacute;  	 	</li>
                      <li>    Gilles Duvert	  	 	</li>
		      <li>    Franz Elsner			</li>
                      <li>    Hans Kristian Eriksen   		</li>
                      <li>    Pablo Fosalba	   		</li>
                      <li>    Silvia 		Galli   	</li>
                      <li>    Ken 		Ganga   	</li>
                      <li>    Tuhin 		Ghosh   	</li>
                      <li>    Giovanna 		Giardino   	</li>
                      <li>    Jean-Christophe 	Hamilton   	</li>
                      <li>    Andrew 		Jaffe   	</li>
                      <li>    Tess 		Jaffe   	</li>
		      <li>    Reijo 		Keskitalo 	</li>
                      <li>    Alex Kim   			</li>
                    </ul>
                </td>
<!--
                <td width="1">
                  &nbsp;
                </td>
-->
                <td width="350" valign="top">
                    <ul>
                      <li>    Ted Kisner   			</li>
		      <li>    Masakazu A.R. Kobayashi		</li>
		      <li>    Eiichiro Komatsu			</li>
		      <li>    Wayne Landsman			</li>
                      <li>    Guy Le Meur   			</li>
		      <li>    Samuel 		Leach		</li>
                      <li>    Antony Lewis   			</li>
                      <li>    Davide Maino   			</li>
		      <li>    Jean-Baptiste Marquette		</li>
		      <li>    Marc-Antoine Miville-Deschenes  	</li>
                      <li>    Serge Monkewitz   		</li>
                      <li>    Ji&#345;&iacute; N&aacute;dvorn&iacute;k   			</li>
                      <li>    Paolo Natoli   			</li>
                      <li>    Fabio Noviello   			</li>
		      <li>    Chris North			</li>
                      <li>    Clive Page   			</li>
                      <li>    Tim Pearson   			</li>
                      <li>    William Pence   			</li>
                      <li>    Nicolas Ponthieu   		</li>
                      <li>    Torsti Poutanen   		</li>
                      <li>    Simon Prunet   			</li>
                      <li>    Benoit Revenu   			</li>
		      <li>    Christophe Ringeval		</li>
		      <li>    Cyrille 		Rosset		</li>
                      <li>    Rosa Ruiloba   			</li>
                      <li>    David N. Spergel   		</li>
                      <li>    Radek Stompor   			</li>
                      <li>    Giovanna Tinetti   		</li>
                      <li>    Maurizio Tomasi   		</li>
                      <li>    Wen Zhao   			</li>
                    </ul>
                </td>
              </tr>
            </table>


</p>

<a name="website">
<h2>About this Web Site</h2>
</a>
<p>
			Website content developed by the <a href="credits.php#team">HEALPix Team</a><br />
	<!--
                        HEALPix documentation is a part of the HEALPix project and is released under the GNU General Public License<br />
-->
			PHP scripts and CSS layout adapted from <a href="http://gnudatalanguage.sourceforge.net">GDL website</a> and <i>Colourise</i> template by <a href="http://www.styleshout.com/">styleshout.com</a>.
			<i>Sui Generis</i> font downloaded from <a href="http://www.urbanfonts.com">Urbanfonts.com</a><br>
<!--
                        Hosted by <a href="http://sf.net/">Sourceforge.net</a>
-->
</p>


<?php require('_footer.inc.php'); ?>
