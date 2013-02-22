#!/usr/bin/perl
#
# E. Hivon, Aug 2005, adapted from register.p by M. Bartelmann
#           Jan 2006: replaced 'registration' by 'subscription'
#
use CGI;
use POSIX;

$::base = "/home/project-web/healpix/tmp/";
$::back = "http://healpix.sourceforge.net/hiding/public/downloads.php";
$::support = "healpix at jpl.nasa.gov";

my $list = new CGI;
$list -> import_names('R');

if ($R::eadd) {
  if ( (index($R::eadd,'@') == -1) || (index($R::eadd,'.') == -1) || length($R::eadd) < 5){
    &pr_err; # no @ or  no . or too short
  } else {
    &regist;
    &pr_suc;
  }
} else {
  &pr_err;
}

# 

exit 0;

sub regist {
   $::temp = $::base . &filenm;
   open (OUT,"> $::temp") || &failed;
   print OUT "Content-type: text/plain\n";
   print OUT "----------------------------\n";
   print OUT "--- HEALPIX Subscription ---\n";
   print OUT "----------------------------\n";
   print OUT "Name: $R::fnam $R::lnam\n";
   print OUT "E-Mail: $R::eadd\n";
   close(OUT);
}

sub pr_suc {
  my $query = new CGI;
  print $query->header;
  print <<EOP;
<html>
<head>
<meta http-equiv="refresh" content="10; URL=$::back">
<title>Subscription</title>
</head>
<body bgcolor=white>
<h2>Subscription Confirmation</h2>
Your subscription to the HEALPIX update mailing list was successfully received.<br>
Information on Healpix will be sent to $R::fnam $R::lnam at
<tt>$R::eadd</tt>.
<br>
<a href="$::back">Return to download page</a><br>
</body>
</html>
EOP
}

sub pr_err {
  my $query = new CGI;
  print $query->header;
  print <<EOP;
<html>
<head>
<title>Subscription</title>
</head>
<body bgcolor=orange>
<h2>Error!</h2>
Your subscription could not be processed because of incomplete data or invalid e-mail.
Please try again.
<br>
<a href="$::back">Return to download page</a><br>
</body>
</html>
EOP
}


sub failed {
  my $query = new CGI;
  print $query->header;

  print <<EOP;
<html>
<head>
<title>Subscription</title>
</head>
<body bgcolor=red>
<h2>Error!</h2>
Your subscription could not be processed because an internal error
was encountered <br>
	    Please try again or send e-mail to $::support.
<br>
<a href="$::back">Return to download page</a><br>
</body>
</html>
EOP
  exit;
}

sub filenm {
  my $temp = "healpixReg.";
  $temp .= strftime("%y%b%d%H%M%S",localtime(time));
  my $rand = int(rand(9));
  $temp .= '-' . $rand . '-';
  if (length($R::eadd) >= 3) {
    $temp .= substr($R::eadd,0,3);
  } else {
    $temp .= $R::eadd;
  }
  $temp .= '.txt';
  $temp;
}

