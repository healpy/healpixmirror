<?php require('_header.inc.php'); ?>




<h2>Healpix History</h2>

<p>
              HEALPix  was  originally devised  in early 1997 by 

		<u>Krzysztof M. G&oacute;rski</u> at the
              <a href="http://www.tac.dk/">Theoretical Astrophysics Center (TAC)</a>

              in Copenhagen, Denmark (he is currently at <a href="http://www.jpl.nasa.gov">Jet Propulsion Laboratory, JPL</a>).  Early development
              of the HEALPix concept and initial implementation was made in
              the spring of 1997 in collaborative work of K.M. G&oacute;rski
              with <u>Eric Hivon</u> (currently at the

              <!-- <a href="http://www.caltech.edu">California Institute of Technology</a>). -->
              <a href="http://www.iap.fr">Institut d&#039;Astrophysique 
									  de Paris, IAP</a>, France).

              <u>Benjamin D. Wandelt</u>, then at the TAC (now at the

              <a href="http://illinois.edu/index.html">University of Illinois at Urbana-Champaign</a> 
              and <a href="http://www.iap.fr">IAP</a>),

              has contributed critically to the further development of HEALPix
              and related mathematical methods. <u>Frode K. Hansen</u>, then at the
              TAC (now at

              <!-- <a href="http://www2.uniroma2.it/">Universita Roma II</a>), -->
              <a href="http://www.uio.no/">Universitetet i Oslo</a>, UIO)

              <u>Anthony J. Banday</u>
              (then at <a href="http://www.mpa-garching.mpg.de/">Max Planck Institut f&#252r Astrophysik (MPA)</a>,

              Garching, Germany, now at <a href="http://www.cesr.fr/">Centre d&#039;Etude 
	       Spatiale du Rayonnement</a>, Toulouse, France) 

	     and <u>Matthias Bartelmann</u> (then at MPA, now at
 		
		<a href="http://www.ita.uni-heidelberg.de"> Institut f&#252r Theoretische Astrophysik</a>, 
 		Heidelberg, Germany)


		provided invaluable input required to make possible
              the first large release of the HEALPix software (Version 1.10).
              <br>
              <br>
              The following releases of the HEALPix software (Versions 1.20 to 2.2) and the current one (Version 3.0) benefited
              from:
		the continuing dedicated effort of E. Hivon, 
		<u>Martin Reinecke</u> (MPA), and A. J. Banday, 
		the work of 

              <u>William O&#039;Mullane</u>
		(<a href="http://www.esa.int/About_Us/ESAC">European Space Astronomy Center, ESAC</a>), 

		<u>Hans Kristian Eriksen</u> 
		(<a href="http://www.uio.no/">Universitetet i Oslo</a>),

		<u>Snorre Boasson</u> 
		(<a href="http://www.ntnu.no/">NTNU</a>),

		<u>David Larson</u> 
		(<a href="http://www.jhu.edu/">Johns Hopkins University, JHU</a>),

		<u>Emmanuel Joliet</u> (ESAC),

		<u>Paddy Leahy</u> (<a href="http://phweb.ph.man.ac.uk/">Manchester University</a>),

		<u>Graca Rocha</u> (<a href="http://www.ipac.caltech.edu/">IPAC</a>),
               the inclusion of <u>Cyrille Rosset</u>&#039;s 
		(APC) and <u>Andrea Zonca</u>&#039;s 
		(UCSB)
		healpy package,

              and numerous contributions
              made by HEALPix users (see the

              <a href="#acknowledgments">Acknowledgments</a>).

              <br>
              <br>
              The active support of HEALPix currently involves the following people:
		<ul>
		<li>
                E. Hivon:
		IDL and F90 developments, release coordination, web site
		</li>
		<li>
              M. Reinecke:
		C, C++ and Java developments
		</li>
		<li>

              C. Rosset: Python
		</li>
		<li>

              A. Zonca: Python
		</li>
		<li>

		E. Joliet:		Java
		</li>
		<li>

              W. O&#039;Mullane:       Java
		</li>
		<li>

              K.M. G&oacute;rski
		</li>
		<li>
              <!-- a href="http://www.mpa-garching.mpg.de/~banday"-->
                A.J. Banday
              <!--/a-->

		</li>
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
		significantly helped us to improve Healpix throughout its several releases

            <table cellSpacing=0 cellPadding=0 border=0 width=605>
              <tr valign="top">
                <td width="300" valign="top">
                    <ul>
                      <li>    Reza Ansari    			</li>
                      <li>    Marc Ashdown   			</li>
                      <li>    Carlo Baccigalupi   		</li>
		      <li>    Karim 		Benabed   	</li> 
		      <li>    Jean-Philippe 	Bernard	  	</li>
                      <li>    Snorre Boasson   			</li>
		      <li>    Caroline 		Bot		</li>
                      <li>    Michela Botti   			</li>
		      <li>    Anthony 		Challinor 	</li>
                      <li>    St&eacute;phane Colombi   	</li>
                      <li>    Giancarlo de Gasperis   		</li>
		      <li>    Clive Dickinson     		</li>
                      <li>    Herv&eacute; Dole   		</li>
                      <li>    Olivier Dor&eacute;  	 	</li>
                      <li>    Hans Kristian Eriksen   		</li>
                      <li>    Pablo Fosalba	   		</li>
                      <li>    Ken Ganga   			</li>
                      <li>    Giovanna Giardino   		</li>
                      <li>    Jean-Christophe Hamilton   	</li>
                      <li>    Andrew Jaffe   			</li>
		      <li>    Reijo 		Keskitalo 	</li>
                      <li>    Alex Kim   			</li>
                      <li>    Ted Kisner   			</li>
		      <li>    Masakazu A.R. Kobayashi		</li>
                    </ul>
                </td>

                <td width="5">
                  &nbsp;
                </td>

                <td width="300" valign="top">
                    <ul>

		      <li>    Eiichiro Komatsu			</li>
		      <li>    Wayne Landsman			</li>
                      <li>    Guy Le Meur   			</li>
		      <li>    Samuel 		Leach		</li>
                      <li>    Anthony Lewis   			</li>
                      <li>    Davide Maino   			</li>
		      <li>    Marc-Antoine Miville-Deschenes  	</li>
                      <li>    Serge Monkewitz   		</li>
                      <li>    Paolo Natoli   			</li>
                      <li>    Clive Page   			</li>
                      <li>    Tim Pearson   			</li>
                      <li>    William Pence   			</li>
                      <li>    Nicolas Ponthieu   		</li>
                      <li>    Torsti Poutanen   		</li>
                      <li>    Simon Prunet   			</li>
                      <li>    Benoit Revenu   			</li>
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

<?php require('_footer.inc.php'); ?>
