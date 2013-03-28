<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">

<head>
  <link rel="shortcut icon" href="images/healpix.ico">
<?php 
  $menu = array(
    'index.php' => 'Features',
    'gallery.php' => 'Gallery',
    'resources.php' => 'Resources',
    'downloads.php' => 'Getting HEALPix',
    'documentation.php' => 'Documentation',
    'support.php' => 'Support',
    'credits.php' => 'Credits'
  );
  echo('<title>HEALPix');
  foreach ($menu as $file => $name) 
   if (basename($_SERVER['SCRIPT_FILENAME']) == $file) echo(' - ' . $name);
  echo('</title>');

?>
  <meta name="description" content="Hierarchical Equal Area Iso Latitude pixelation of the sphere (HEALPix); Data Analysis, Simulations and Visualization on the Sphere" />
  <meta http-equiv="content-type" content="application/xhtml+xml; charset=UTF-8" />
  <link rel="stylesheet" href="images/Healpix.css" type="text/css" />


<!-- Google analytics -->
<?php 
  include_once("_analytics_tracking.php");
?>
</head>

<body>


<!-- wrap starts here -->
<div id="wrap">

   <!--header -->
    <div id="header">			
				
   <h1 id="logo-text"><a>HEALPix</a></h1>		
    <p id="intro">
      Data Analysis, Simulations and Visualization on the Sphere
   <br />
   </p>	
		
  <div  id="nav">
   <ul>
<?php 
  $menu = array(
    'index.php' => 'Features',
    'gallery.php' => 'Gallery',
    'resources.php' => 'Resources',
    'downloads.php' => 'Getting HEALPix',
    'documentation.php' => 'Documentation',
    'support.php' => 'Support',
    'credits.php' => 'Credits'
  );
  foreach ($menu as $file => $name) 
  {
    echo('<li');
    if (basename($_SERVER['SCRIPT_FILENAME']) == $file) echo(' id="current"');
    echo('><a href="' . $file . '">' . $name . '</a></li>');
  }
?>
			</ul>		
		</div>	
		
				
	<!--header ends-->					
	</div>
	
	<!-- content-wrap starts -->
	<div id="content-wrap">
		<div id="main">
			<a name="TemplateInfo"></a>
