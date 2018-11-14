<!DOCTYPE html>

<html lang="en">

<head>
  <meta charset="utf-8">
  <!-- link rel="shortcut icon" href="images/healpix.ico" -->
  <link rel="apple-touch-icon" sizes="180x180" href="images/favicons/apple-touch-icon.png?v=2017">
  <link rel="icon" type="image/png" sizes="32x32" href="images/favicons/favicon-32x32.png?v=2017">
  <link rel="icon" type="image/png" sizes="16x16" href="images/favicons/favicon-16x16.png?v=2017">
  <link rel="manifest" href="images/favicons/manifest.json?v=2017">
  <link rel="mask-icon" href="images/favicons/safari-pinned-tab.svg?v=2017" color="#5bbad5">
  <link rel="shortcut icon" href="images/favicons/favicon.ico?v=2017">
  <meta name="apple-mobile-web-app-title" content="HEALPix">
  <meta name="application-name" content="HEALPix">
  <meta name="msapplication-config" content="images/favicons/browserconfig.xml?v=2017">
  <meta name="theme-color" content="#ffffff">

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
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="images/Healpix.css" type="text/css" />


<!-- Google analytics -->
<?php
#$myhost = parse_url($_SERVER['SERVER_NAME'], PHP_URL_HOST);
$myhost = $_SERVER['SERVER_NAME'];
$httphost  = 'healpix.sourceforge.net';
$httpshost = 'healpix.sourceforge.io';
echo ("<!--  $myhost ==  $httphost, $httpshost  -->\r\n");
if ($myhost == $httphost) include_once("_analytics_tracking.php");
if ($myhost == $httpshost) include_once("_gtag.php");
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
			<a id="TemplateInfo"></a>
