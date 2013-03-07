			<br />	

		<!-- main ends -->	
		</div>
		
		<!-- sidebar starts -->
		<div id="sidebar">
		
			<h3>Latest news</h3>
<?php require('_news.inc.php'); ?>

<!--
			<h3>Some stats (ohloh.net)</h3>
<div class="ohloh" style="margin-top:10px">
<script type="text/javascript" src="http://www.ohloh.net/p/40087/widgets/project_basic_stats.js"></script>
</div>
-->

<h3>Search Box</h3>	
	<form action="http://www.google.com/search" class="searchform" method="get">
	<table width=100% border="0" cellpadding="0" cellspacing="0">
	<tr align=center><td colspan=2>
	<input type="text"   name="q" size="32" maxlength="255" value="" />
	</td></tr>
	<tr align=center><td colspan=2>
	<input class="button" type="submit" value="Search" />
	<input class="button" type="reset" value="Clear" />
	</td></tr>
	<tr><td align="center" style="font-size:100%">
	<input type="checkbox"  name="sitesearch"
	 value="healpix.sourceforge.net" checked /> only search this site<br />
	</td></tr></table>
	</form>		

<!--
Read more at http://www.askdavetaylor.com/how_can_i_add_a_google_search_box_to_my_web_site.html#pi7uw7S71w8KhJtm.99
-->
			
						
		<!-- sidebar ends -->		
		</div>
		
	<!-- content-wrap ends-->	
	</div>
		
	<!-- footer starts here -->	
	<div id="footer-wrap"><div id="footer-content">
	
		<p>
<!--
			Website content is developed by <a href="credits.php">The HEALPix Team</a><br />
                        HEALPix documentation is a part of the HEALPix project and is released under the GNU General Public License<br />
			Website layout based on the <a href="http://gnudatalanguage.sourceforge.net">GDL website</a> layout and <strong>Colourise</strong> template by <a href="http://www.styleshout.com/">styleshout.com</a><br />
			Sui Generis font downloaded from <a href="http://www.urbanfonts.com">Urbanfonts.com</a><br>
                        Hosted by <a href="http://sf.net/">Sourceforge.net</a>
-->
<a href="credits.php#website">About this Web Site</a>

<?php 
if (version_compare(PHP_VERSION,'5.1.0') >=0){
  $tz = 'UTC';
  date_default_timezone_set($tz);
  $f = basename($_SERVER['SCRIPT_FILENAME']); # file including this footer
  echo ('<br />Last Edited on ' . date("Y-m-d H:i", filemtime($f)) . ' '  . $tz );
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
