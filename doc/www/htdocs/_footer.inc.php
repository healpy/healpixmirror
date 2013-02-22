			<br />	

		<!-- main ends -->	
		</div>
		
		<!-- sidebar starts -->
		<div id="sidebar">
		
			<h3>What&#039;s new</h3>
<?php require('_news.inc.php'); ?>

<!--
			<h3>Some stats (ohloh.net)</h3>
<div class="ohloh" style="margin-top:10px">
<script type="text/javascript" src="http://www.ohloh.net/p/40087/widgets/project_basic_stats.js"></script>
</div>
-->

<!--
			<h3>Search Box</h3>	
			<form action="#" class="searchform">
				<p>
				<input name="search_query" class="textbox" type="text" />
  				<input name="search" class="button" value="Search" type="submit" />
				</p>			
			</form>		
-->
			
						
		<!-- sidebar ends -->		
		</div>
		
	<!-- content-wrap ends-->	
	</div>
		
	<!-- footer starts here -->	
	<div id="footer-wrap"><div id="footer-content">
	
		<p>
			Website content is developed by <a href="credits.php">The Healpix Team</a><br />
                        HEALPix documentation is a part of the HEALPix project and is released under the GNU General Public License<br />
			Website layout based on the <a href="http://gnudatalanguage.sourceforge.net">GDL website</a> layout and <strong>Colourise</strong> template by <a href="http://www.styleshout.com/">styleshout.com</a><br />
			Sui Generis font downloaded from <a href="http://www.urbanfonts.com">Urbanfonts.com</a><br>
                        Hosted by <a href="http://sf.net/">Sourceforge.net</a>

<?php 
if (version_compare(PHP_VERSION,'5.1.0') >=0){
  $tz = 'UTC';
  date_default_timezone_set($tz);
  $f = basename($_SERVER['SCRIPT_FILENAME']); # file including this footer
  echo ('<br>Last Edited on ' . date("Y-m-j h:i", filemtime($f)) . ' '  . $tz );
}
?>
</p>
		
<!--
		<p class="float-right">
              		<a href="http://jigsaw.w3.org/css-validator/check/referer">CSS</a> |
               		<a href="http://validator.w3.org/check/referer">XHTML</a>
		</p>
-->
			
	</div></div>
	<div class="clearer"></div>
	<!-- footer ends here -->

<!-- wrap ends here -->
</div>

</body>
</html>
