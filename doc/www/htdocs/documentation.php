<?php require('_header.inc.php'); ?>

<p>The HEALPix documentation is currently available for downloading in <a href="pdf/pdf_index.pdf"
    onClick="gtag('send', 'event', 'PDF', 'Download', 'PDF Index');">pdf files</a>, 
    in a <a href="epub/HEALPix/Documentation.epub"
    onClick="gtag('send', 'event', 'EPUB', 'Download', 'EPUB Documentation');">single epub file</a>
    and can be browsed in <a href="html/main.htm">html</a>

              
<?php
function doc_entry($de_title, $de_htmlfile, $de_pdffile, $de_gatag)
# create entry of (optional PDF file) and html file
{echo '<tr>   ';
 echo ' <td style="width:285px"> ' . $de_title . "</td> \n" ;
 if (strlen($de_pdffile) > 0) {
   echo ' <td style="width:75px;  text-align:center">';
   echo '     <a href="' . $de_pdffile . '" target="_top"';
   echo '       onClick="gtag(\'send\', \'event\', \'PDF\', \'Download\', \'' . $de_gatag . '\');" ';
   echo '     >pdf</a> ';
   echo '   <br> ';
   echo "  (" . number_format(filesize($de_pdffile)/1024/1024,2) . " MiB)" ;
   echo " </td> \n" ;
 } else {
   echo ' <td style="width:75px;  text-align:center">    &nbsp;     </td> ' . "\n";
 }
 echo ' <td style="width:75px;  text-align:center">' ;
 echo '     <a href="' . $de_htmlfile .'" target="_top">html</a> ' ;
 echo '    <br><br> ';
 echo " </td> </tr> \n";
}
?>


<?php
function doc_entry2($de_title, $de_htmlfile, $de_pdffile, $de_epubfile, $de_gatag, $de_egatag)
# create entry of (optional PDF file) and html file
{echo '<tr>   ';
 echo ' <td style="width:285px"> ' . $de_title . "</td> \n" ;
 if (strlen($de_pdffile) > 0) {
   echo ' <td style="width:75px;  text-align:center">';
   echo '     <a href="' . $de_pdffile . '" target="_top"';
   echo '       onClick="gtag(\'send\', \'event\', \'PDF\', \'Download\', \'' . $de_gatag . '\');" ';
   echo '     >pdf</a> ';
   echo '   <br> ';
   echo "  (" . number_format(filesize($de_pdffile)/1024/1024,2) . " MiB)" ;
   echo " </td> \n" ;
 } else {
   echo ' <td style="width:75px;  text-align:center">    &nbsp;     </td> ' . "\n";
 }
 if (strlen($de_epubfile) > 0) {
   echo ' <td style="width:75px;  text-align:center">';
   echo '     <a href="' . $de_epubfile . '" target="_top"';
   echo '       onClick="gtag(\'send\', \'event\', \'EPUB\', \'Download\', \'' . $de_egatag . '\');" ';
   echo '     >epub</a> ';
   echo '   <br> ';
   echo "  (" . number_format(filesize($de_epubfile)/1024/1024,2) . " MiB)" ;
   echo " </td> \n" ;
 } else {
   echo ' <td style="width:75px;  text-align:center">    &nbsp;     </td> ' . "\n";
 }
 echo ' <td style="width:75px;  text-align:center">' ;
 echo '     <a href="' . $de_htmlfile .'" target="_top">html</a> ' ;
 echo '    <br><br> ';
 echo " </td> </tr> \n";
}
?>


<?php clearstatcache();?>

<table style="width:585px">

<!--  begin list -->
<?php doc_entry2("The HEALPIX primer", "html/intro.htm", 
		 "pdf/intro.pdf", 
		 "epub/HEALPixDocumentation.epub",
		 "PDF Introduction",  
		 "EPUB Introduction") ?>
<tr><td> &nbsp; </td><td> &nbsp; </td><td> &nbsp; </td></tr>

<?php doc_entry2("HEALPIX Facility Installation Guidelines", "html/install.htm", 
		 "pdf/install.pdf", 
		 "epub/HEALPixDocumentation.epub", 
		 "PDF Installation",  
		 "EPUB Installation") ?>
<tr><td> &nbsp; </td><td> &nbsp; </td><td> &nbsp; </td></tr>

<?php doc_entry2("HEALPIX Fortran90 Facilities User Guidelines", "html/facilities.htm", 
		 "pdf/facilities.pdf", 
		 "epub/HEALPixDocumentation.epub", 
		 "PDF F90 Facilities",
		 "EPUB F90 Facilities")?>
<tr><td> &nbsp; </td><td> &nbsp; </td><td> &nbsp; </td></tr>

<?php doc_entry2("HEALPIX Fortran90 Subroutines Overview", "html/subroutines.htm", 
		 "pdf/subroutines.pdf", 
		 "epub/HEALPixDocumentation.epub", 
		 "PDF F90 Subroutines",
		 "EPUB F90 Subroutines") ?>
<tr><td> &nbsp; </td><td> &nbsp; </td><td> &nbsp; </td></tr>

<?php doc_entry2("HEALPIX IDL Facilities Overview", "html/idl.htm", 
		 "pdf/idl.pdf", 
		 "epub/HEALPixDocumentation.epub", 
		 "PDF IDL Facilities",
		 "EPUB IDL Facilities") ?>
<tr><td> &nbsp; </td><td> &nbsp; </td><td> &nbsp; </td></tr>

<?php doc_entry2("HEALPIX C Subroutines Overview", "html/csub.htm", 
		 "pdf/csub.pdf", 
		 "epub/HEALPixDocumentation.epub", 
		 "PDF C Subroutines Overview",
		 "EPUB C Subroutines Overview") ?>
<tr><td> &nbsp; </td><td> &nbsp; </td><td> &nbsp; </td></tr>

<?php doc_entry2("HEALPIX C++ Facilities and Subroutines Overview", "html/index_cxx.htm", 
		 "", "", "", "") ?>
<tr><td> &nbsp; </td><td> &nbsp; </td><td> &nbsp; </td></tr>

<?php doc_entry2("HEALPix Java Overview", "html/java/index.html", 
		 "", "", "", "") ?>
<tr><td> &nbsp; </td><td> &nbsp; </td><td> &nbsp; </td></tr>

<?php doc_entry2("HEALPix Python Overview (healpy)", "https://healpy.readthedocs.io/en/latest", 
		 "", "", "", "") ?>
<!--   end list -->

</table>



<?php require('_footer.inc.php'); ?>
