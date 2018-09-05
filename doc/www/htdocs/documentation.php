<?php require('_header.inc.php'); ?>

<p>The HEALPix documentation is currently available for downloading in <a href="pdf/pdf_index.pdf"
  onClick="gtag('send', 'event', 'PDF', 'Download', 'PDF Index');"
>pdf</a> and can be browsed in <a href="html/main.htm">html</a>

              
<?php
function doc_entry($de_title, $de_htmlfile, $de_pdffile, $de_gatag)
# create entry of (optional PDF file) and html file
{echo '<tr>   ';
 echo ' <td width="285"> ' . $de_title . "</td> \n" ;
 if (strlen($de_pdffile) > 0) {
   echo ' <td width="75" align="center">';
   echo '     <a href="' . $de_pdffile . '" target="_top"';
   echo '       onClick="gtag(\'send\', \'event\', \'PDF\', \'Download\', \'' . $de_gatag . '\');" ';
   echo '     >pdf</a> ';
   echo '   <br> ';
   echo "  (" . number_format(filesize($de_pdffile)/1024/1024,2) . " MiB)" ;
   echo " </td> \n" ;
 } else {
   echo ' <td width="75" align="center">    &nbsp;     </td> ' . "\n";
 }
 echo ' <td width="75" align="center">' ;
 echo '     <a href="' . $de_htmlfile .'" target="_top">html</a> ' ;
 echo '    <br><br> ';
 echo " </td> </tr> \n";
}
?>

<?php clearstatcache();?>

<table cellSpacing=0 cellPadding=0 border=0 width=585>

<!--  begin list -->
<?php doc_entry("The HEALPIX primer", "html/intro.htm", "pdf/intro.pdf", "PDF Introduction") ?>
<tr><td> &nbsp; </td></tr>

<?php doc_entry("HEALPIX Facility Installation Guidelines", "html/install.htm", "pdf/install.pdf", "PDF Installation") ?>
<tr><td> &nbsp; </td></tr>

<?php doc_entry("HEALPIX Fortran90 Facilities User Guidelines", "html/facilities.htm", "pdf/facilities.pdf", "PDF F90 Facilities") ?>
<tr><td> &nbsp; </td></tr>

<?php doc_entry("HEALPIX Fortran90 Subroutines Overview", "html/subroutines.htm", "pdf/subroutines.pdf", "PDF F90 Subroutines") ?>
<tr><td> &nbsp; </td></tr>

<?php doc_entry("HEALPIX IDL Facilities Overview", "html/idl.htm", "pdf/idl.pdf", "PDF IDL Facilities") ?>
<tr><td> &nbsp; </td></tr>

<?php doc_entry("HEALPIX C Subroutines Overview", "html/csub.htm", "pdf/csub.pdf", "PDF C Subroutines Overview") ?>
<tr><td> &nbsp; </td></tr>

<?php doc_entry("HEALPIX C++ Facilities and Subroutines Overview", "html/index_cxx.htm", "", "") ?>
<tr><td> &nbsp; </td></tr>

<?php doc_entry("HEALPix Java Overview", "html/java/index.html", "", "") ?>
<tr><td> &nbsp; </td></tr>

<?php doc_entry("HEALPix Python Overview (healpy)", "https://healpy.readthedocs.io/en/latest", "", "") ?>
<!--   end list -->

</table>
</p>



<?php require('_footer.inc.php'); ?>
