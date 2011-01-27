# -*- perl -*-
# This attempts to define necessary translations for the HEALPix 
# style files. (cf healpix.sty and healrut.sty)  
#
# Eric Hivon & Ted Kisner

package main;

print "Using HEALPix Style Conversions...\n";

#-------------------------
# from python.perl
sub next_argument{
    my $param;
    $param = missing_braces()
      unless ((s/$next_pair_pr_rx/$param=$2;''/eo)
              ||(s/$next_pair_rx/$param=$2;''/eo));
    return $param;
}

sub use_wrappers($$$){
    local($_,$before,$after) = @_;
    my $stuff = next_argument();
    return $before . $stuff . $after . $_;
}
#--------------------------
sub do_cmd_optional {
    return use_wrappers($_[0], '<i><font color="\#224422">', '</font></i>'); }

sub do_cmd_nosectionname{
    local($_) = @_;
    " " . $_;
}

sub do_cmd_setlength{}

sub do_cmd_warnhtml {
    local($_) = @_;
    $color= "CC0000"; # dark red
    $mess  ="This page contains many equations that may not show up correctly in HTML.\n";
    $mess .="We recommend that the Postscript document be used instead.\n";
    "<b><font size=+3><FONT COLOR=\"\#$color\">" . $mess . "</FONT></font></b><br>" . $_;
}

#---------
sub do_cmd_docid {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_doc) = &translate_commands($&);
    $color= "CC0000"; # dark red
    $t_docid = "<FONT COLOR=\"\#$color\">" . $t_doc . "</FONT>";
    $_;
}

sub do_cmd_thedocid {
    local($_) = @_;
    $t_docid . $_ ;
}
#---------------
sub do_cmd_docrv {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_docrv) = &translate_commands($&);
    $_;
}
sub do_cmd_thedocrv {
    local($_) = @_;
    $t_docrv . $_ ;
}
#---------

sub do_cmd_healpix{
  local($_) = @_;
  $out = '<b>HEALPix</b>';
  join('',$out,' ',$_);
}

sub do_cmd_healpixns{
  local($_) = @_;
  $out = '<b>HEALPix</b>';
  join('',$out,$_);
}

sub do_cmd_filenamelen{
  local($_) = @_;
  "<tt>filenamelen</tt>" . $_;
}

#---------

sub do_cmd_underline {
    return use_wrappers($_[0], '<u>', '</u>'); }

sub do_cmd_default {
    local($_) = @_;
    "<b>default:</b>" . $_;
}

sub do_cmd_nodefault {
    local($_) = @_;
    "<b> (default : none)</b>" . $_;
}

sub do_cmd_seealso {
    local($_) = @_;
    "<i>see also:</i>" . $_;
}

sub do_cmd_fileparam {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_param) = &translate_commands($&);
    "<tt>$t_param</tt><br>$_";
}

sub do_cmd_mycode {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_code) = &translate_commands($&);
    "<tt>  &nbsp;&nbsp;&nbsp;&nbsp; $t_code &nbsp;&nbsp;&nbsp;&nbsp; </tt> $_";
}

#-----------
sub do_env_qualifiers_hpx2gs {
    local($_) = @_;
    "<H1>QUALIFIERS</H1><BR>" . $_ ;
}

sub do_env_qualifiers_mollview {
    local($_) = @_;
    "<H1>QUALIFIERS</H1><BR>" . $_ ;
}

sub do_env_keywords_mollview {
    local($_) = @_;
    "<H1>KEYWORDS</H1><BR>" . $_ ;
}
#---------

%ImageMarks = (
	'BlueBall',		'blueball',
	'RedBall',		'redball',
	'OrangeBall',		'orangeball',
	'GreenBall',		'greenball',
	'PinkBall',		'pinkball',
	'PurpleBall',		'purpleball',
	'WhiteBall',		'whiteball',
	'YellowBall',		'yellowball',
	);

%ImageSizeMarks = (
	'BlueBall',		'WIDTH="14" HEIGHT="14"',
	'RedBall',		'WIDTH="14" HEIGHT="14"',
	'OrangeBall',		'WIDTH="14" HEIGHT="14"',
	'GreenBall',		'WIDTH="14" HEIGHT="14"',
	'PinkBall',		'WIDTH="14" HEIGHT="14"',
	'PurpleBall',		'WIDTH="14" HEIGHT="14"',
	'WhiteBall',		'WIDTH="14" HEIGHT="14"',
	'YellowBall',		'WIDTH="14" HEIGHT="14"',
	);

#
#  The htmllist environment is equivalent to the description
#  environment in the printed copy, but produces bold descriptions
#  with optional image marks in the HTML version.
#
#  Example:
#
#	\begin{htmllist}[WhiteBall]
#	\item[Item 1:] This will have a white ball
#	\item[Item 2:] This will also have a white ball
#	\htmlitemmark{RedBall}
#	\item[Item 3:] This will have a red ball
#	\end{htmllist}
#

#-------------------------------

sub do_env_facility {
    local($_) = @_;
    local($descr) = &get_next_argument;
    "<b><font size=+6>$t_docid</font></b><hr><H2>$descr</H2><br>Location in HEALPix directory tree: <b>$_</b>";
}
# sub do_env_facility {
#     local($_) = @_;
#     local($descr) = &get_next_argument;
#     "<b><font size=+6>$t_docid</font></b><hr>$descr<br>Location in HEALPix directory tree: <b>$_</b>";
#}

sub do_env_f90format {
    local($_) = @_;
    "<hr><h1>FORMAT </h1><blockquote><h2>call $t_docid($_)</h2></blockquote>";
}

sub do_env_f90facility {
    local($_) = @_;
    "<hr><h1>FORMAT </h1><blockquote><h2>%$_</h2></blockquote>";
}

sub do_env_f90function {
    local($_) = @_;
    "<hr><h1>FORMAT </h1><h2>var=$t_docid($_)</h2>";
}

sub do_env_Cfunction {
    local($_) = @_;
    "<hr><h1>FORMAT </h1><h2>$_</h2>";
}

sub do_env_Cfacility {
    local($_) = @_;
    "<hr><h1>FORMAT </h1><h2>$_</h2>";
}

sub do_env_IDLformat {
    local($_) = @_;
    "<hr><h1>FORMAT </h1><h2>IDL>$_</h2>";
}

sub do_env_recommend {
    local($_) = @_;
    "<hr><H1>RECOMMENDATIONS FOR USERS</H1><blockquote>$_</blockquote>";
}

sub do_env_arguments {
    local($_) = @_;
    "<hr><H1>ARGUMENTS</H1>$_";
}

sub do_env_codedescription{
    local($_) = @_;
    "<hr><H1>DESCRIPTION</H1><BR> <blockquote>" . $_ . "</blockquote>";
}

sub do_env_cd_contd{
    local($_) = @_;
    "<blockquote>" . $_ . "</blockquote>";
}

sub do_env_messages {
    local($_) = @_;
    local($the_docid) = '';
    if ($t_docid) {
	$the_docid .= "<b>$t_docid</b>\n";
    } else {
  	$the_docid .= "<b>I don t know</b>";
    }
    "<hr><H1>MESSAGES</H1><h2>This section describes error messages generated by $the_docid</h2>" . $_ ;
}

sub do_env_datasets {
    local($_) = @_;
    local($the_docid) = '';
    if ($t_docid) {
	$the_docid .= "<b>$t_docid</b>\n";
    } else {
  	$the_docid .= "<b>I don t know</b>";
    }
    "<hr><H1>DATASETS</H1><h2> The following datasets are involved in the $the_docid processing.</h2>" . $_ ;
}

sub do_env_example {
    local($_) = @_;
    $t_example = &missing_braces unless
	((s/$next_pair_pr_rx/$t_example = $2; ''/eo)
	||(s/$next_pair_rx/$t_example = $2; ''/eo));
    $t_bla = &missing_braces unless
	((s/$next_pair_pr_rx/$t_bla = $2; ''/eo)
	||(s/$next_pair_rx/$t_bla = $2; ''/eo));
#      "<HR><H1>EXAMPLE:</H1><FONT COLOR=\"\#00CC00\"><FONT SIZE=+2>$t_example</font></font><BR><blockquote>$t_bla</blockquote>" ;
    "<HR><H1>EXAMPLE:</H1><tt><FONT SIZE=+1>$t_example</font></tt><BR><blockquote>" . $t_bla . "</blockquote>" ;
}

sub do_env_examples {
    local($_) = @_;
    $t_num = &missing_braces unless
	((s/$next_pair_pr_rx/$t_num = $2; ''/eo)
	||(s/$next_pair_rx/$t_num = $2; ''/eo));
    $t_example = &missing_braces unless
	((s/$next_pair_pr_rx/$t_example = $2; ''/eo)
	||(s/$next_pair_rx/$t_example = $2; ''/eo));
    $t_bla = &missing_braces unless
	((s/$next_pair_pr_rx/$t_bla = $2; ''/eo)
	||(s/$next_pair_rx/$t_bla = $2; ''/eo));
#      "<hr><H1>EXAMPLE # $t_num:</H1><FONT COLOR=\"\#00CC00\"><FONT SIZE=+2>$t_example</font></font><BR><blockquote>$t_bla</blockquote>" ;
    "<hr><H1>EXAMPLE # $t_num:</H1><tt><FONT SIZE=+1>$t_example</font></tt><br><blockquote>$t_bla</blockquote>" ;
}


#------------------------

sub do_env_qualifiers{
    local($_) = @_;
    "<hr><H1>QUALIFIERS</H1><BR> " . $_;
}

sub do_env_options{
    local($_) = @_;
    "<hr><H1>COMMAND LINE OPTIONS</H1><BR> " . $_;
}

sub do_env_related{
    local($_) = @_;
    "<hr><H1>RELATED ROUTINES</H1><h3>This section lists the routines related to <b>$t_docid</b></h3><BR> " . $_;
}

sub do_env_release{
    local($_) = @_;
    "<hr><H1>RELEASE NOTES</H1><blockquote>$_</blockquote>";
}

sub do_env_keywords{
    local($_) = @_;
    "<hr><H1>KEYWORDS</H1><BR> " . $_;
}

sub do_env_modules{
    local($_) = @_;
    "<hr><H1>MODULES & ROUTINES</H1><H2>This section lists the modules and routines used by $t_docid.</h2>" . $_; 
}

sub do_env_support{
    local($_) = @_;
    $blabla = "<H1>SUPPORT    </H1><H2>This section lists those routines and facilities (including those <i>external</i> to the Healpix distribution) which can assist in the utilisation of <b>$t_docid</b>.</H2><br>";
    "<hr>" . $blabla . $_;
}

sub do_env_qulist{
  &do_env_mylist(@_," COMPACT");
}

sub do_env_qulistwide{
  &do_env_mylist(@_," COMPACT");
}

sub do_env_optionlistwide{
  &do_env_mylist(@_," COMPACT");
}

sub do_env_sulist{
  &do_env_mylist(@_," COMPACT");
}

sub do_env_kwlist{
  &do_env_mylist(@_," COMPACT");
}

sub do_env_relist{
  &do_env_mylist_bullet(@_," COMPACT", "BlueBall");
}

sub do_env_itemize{
  &do_env_mylist_bullet(@_," COMPACT", "GreenBall");
}

#--------------------
# copied from htmllist
sub do_env_myliststar{
  &do_env_mylist(@_," COMPACT");
}

sub set_mylist_marker {
    local($icon) = @_;
    local($ICONSERVER) = ($LOCAL_ICONS ? '' : $ICONSERVER.$dd );
    if (!($ImageMarks{$icon})) {
	print "\nUnknown icon '$icon' for mylist marker";
	&write_warnings("Unknown icon '$icon' for mylist marker");
	return();
    }
    local($mark_size,$imagemark) = $ImageSizeMarks{$icon};
    $icon = "$ICONSERVER$ImageMarks{$icon}.$IMAGE_TYPE" if ($ImageMarks{$icon});
    $imagemark = '<IMG ' . $mark_size . ' SRC="' . $icon . '" ALT="*">';
    $imagemark =~ s/~/&#126;/g;	# Allow ~'s in $ICONSERVER
    # mark as used, in case $LOCAL_ICONS: thanks, Roman E. Pavlov
    $used_icons{$icon} = 1; 
    $imagemark;
}

sub do_env_mylist{
    local($_, $compact,$bullet) = @_;
#      local($bullet,$pat) = &get_next_optional_argument;
    #RRM - catch nested lists
    $_ = &translate_environments($_);
  
    $compact = "" unless $compact;
    local($imagemark,$mark,$item_len,$desc_len,$mark_len,$mark_size);
    $imagemark = &set_mylist_marker($bullet) if ($bullet);

    $* = 1;
    local($Maxlength) = 99999;
    local($i,@items_done);
    print "[";
    while (1) {
	print "*";
	$item_len = $mark_len = $desc_len = $Maxlength;
	$desc_len = length($`) if (/$item_description_rx/);
	$mark_len = length($`) if (/\\htmlitemmark/);
	$item_len = length($`) if (/\\item$delimiter_rx/);
	# exit when none of them match
	last if ($item_len == $Maxlength && $mark_len == $Maxlength
	    && $desc_len == $Maxlength);
	if ($mark_len < $item_len && $mark_len < $desc_len) {
	    if (/\\htmlitemmark/) {
		$_ = $&.$';
		push(@items_done,&translate_commands($`));
		$mark = &missing_braces unless (
		    (s/\\htmlitemmark$any_next_pair_pr_rx/$mark=$2;''/eo)
		    ||(s/\\htmlitemmark$any_next_pair_rx/$mark=$2;''/eo));
		$imagemark = &set_mylist_marker($mark) if ($mark);
	    }
	} elsif ($item_len < $desc_len) {
	    /\\item$delimiter_rx/;
	    push(@items_done,&translate_commands($`),
		    "<DT>$imagemark\n<DD>$1");
		$_=$';
	} else  {
	    /$item_description_rx\s*($labels_rx8)?\s*/;
	    push(@items_done,&translate_commands($`),
		"<DT>$imagemark" 
		. (($9)? "<A NAME=\"$9\">\n<B>$1<\/B><\/A>" : "\n<B>$1<\/B>" ) 
		."\n<DD>");
		$_=$';
	}
    }
    $* = 0;
    $_ = join('',@items_done, $_); undef @items_done;

    #RRM: cannot have anything before the first <LI>
    local($savedRS) = $/; $/='';
    $_ =~ /<D(T|D)>/s;
    local($preitems);
    if ($`) {
	local($preitems) = $`; $_ = $&.$';
	$preitems =~ s/<P( [^>]*)?>//g;
	$preitems = "\n".$preitems if $preitems;
    }
    $/ = $savedRS; $* = 0;	# Multiline matching OFF

    $_ = '<DT>'.$_ unless (/^\s*<D(D|T)/);
    print "]";
    join('',$preitems,"<DL$compact>", $_, '</DL>');
}

# added by EH: list with bullets but no named item
sub do_env_mylist_bullet{
    local($_, $compact,$bullet) = @_;
#      local($bullet,$pat) = &get_next_optional_argument;
    #RRM - catch nested lists
    $_ = &translate_environments($_);
  
    $compact = "" unless $compact;
    local($imagemark,$mark,$item_len,$desc_len,$mark_len,$mark_size);
    $imagemark = &set_mylist_marker($bullet) if ($bullet);

    $* = 1;
    local($Maxlength) = 99999;
    local($i,@items_done);
    print "[";
    while (1) {
	print "*";
	$item_len = $mark_len = $desc_len = $Maxlength;
	$desc_len = length($`) if (/$item_description_rx/);
	$mark_len = length($`) if (/\\htmlitemmark/);
	$item_len = length($`) if (/\\item$delimiter_rx/);
	# exit when none of them match
	last if ($item_len == $Maxlength && $mark_len == $Maxlength
	    && $desc_len == $Maxlength);
	if ($mark_len < $item_len && $mark_len < $desc_len) {
	    if (/\\htmlitemmark/) {
		$_ = $&.$';
		push(@items_done,&translate_commands($`));
		$mark = &missing_braces unless (
		    (s/\\htmlitemmark$any_next_pair_pr_rx/$mark=$2;''/eo)
		    ||(s/\\htmlitemmark$any_next_pair_rx/$mark=$2;''/eo));
		$imagemark = &set_mylist_marker($mark) if ($mark);
	    }
	} elsif ($item_len < $desc_len) {
	    /\\item$delimiter_rx/;
	    push(@items_done,&translate_commands($`),
		    "<DT><DD>$imagemark\n$1");
		$_=$';
	} else  {
	    /$item_description_rx\s*($labels_rx8)?\s*/;
	    push(@items_done,&translate_commands($`),
		"<DT><DD>$imagemark" 
		. (($9)? "<A NAME=\"$9\">\n<B>$1<\/B><\/A>" : "\n<B>$1<\/B>" ) 
		."\n");
		$_=$';
	}
    }
    $* = 0;
    $_ = join('',@items_done, $_); undef @items_done;

    #RRM: cannot have anything before the first <LI>
    local($savedRS) = $/; $/='';
    $_ =~ /<D(T|D)>/s;
    local($preitems);
    if ($`) {
	local($preitems) = $`; $_ = $&.$';
	$preitems =~ s/<P( [^>]*)?>//g;
	$preitems = "\n".$preitems if $preitems;
    }
    $/ = $savedRS; $* = 0;	# Multiline matching OFF

    $_ = '<DT>'.$_ unless (/^\s*<D(D|T)/);
    print "]";
    join('',$preitems,"<DL$compact>", $_, '</DL>');
}


# copied from amstex.perl
#----------
sub do_cmd_title {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_title) = &translate_commands($&);
    $_;
}

sub do_cmd_author {
    local($_) = @_;
    if (/\\endauthor/) {
	$t_author = &translate_commands($`);
	$t_author =~ s/(^\s*|\s*$)//g;
	return($');
    }
    &get_next_optional_argument;
    local($rest) = $_;
    $t_author = &missing_braces unless (
	($rest =~ s/$next_pair_pr_rx/$t_author=$&;''/eo)
	||($rest =~ s/$next_pair_rx/$t_author=$&;''/eo));
    ($t_author) =  &translate_commands($t_author);
    $rest;
}

sub do_cmd_address {
    local($_) = @_;
    if (/\\endaddress/) {
	$t_address = &translate_commands($`);
	$t_address =~ s/(^\s*|\s*$)//g;
	return($');
    }
    &get_next_optional_argument;
    local($rest) = $_;
    $t_address = &missing_braces unless (
	($rest =~ s/$next_pair_pr_rx/$t_address=$&;''/eo)
	||($rest =~ s/$next_pair_rx/$t_address=$&;''/eo));
    ($t_address) =  &translate_commands($t_address);
    $rest;
}

sub do_cmd_abstract {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_abstract) = &translate_commands($&);
    $_;
}

sub do_cmd_date {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    ($t_date) = &translate_commands($&);
    $_;
}

# sub do_cmd_frontpage {
#     local($_) = @_;
#     local($the_title) = '';
#     if ($t_title) {
#         $the_title .= "<H1 ALIGN=CENTER>$t_title</H1>\n";
#     } else { &write_warnings("This document has no title."); }
#     if ($t_abstract) {
#  	$the_title .= "<DIV>$t_abstract</DIV>\n";
#     }
#     if ($t_author) {
#         $the_title .= "<P ALIGN=CENTER><STRONG>$t_author</STRONG></P>\n";
#     } else { &write_warnings("There is no author for this document."); }
#     if (($t_date)&&!($t_date=~/^\s*(($O|$OP)\d+($C|$CP))\s*\1\s*$/)) {
#         $the_title .= "<BR><P ALIGN=CENTER><I>Date:</I> $t_date</P>\n";}
# 
#     $the_title . $_ ;
# }

sub do_cmd_frontpage {
    local($_) = @_;
    local($the_title) = '';
    if ($t_title) {
        $the_title .= "<H1 ALIGN=CENTER>$t_title</H1>\n";
    } else { &write_warnings("This document has no title."); }
    if ($t_abstract) {
 	$the_title .= "<DIV>$t_abstract</DIV>\n";
    }
    if ($t_author) {
        $the_title .= "<P ALIGN=CENTER><STRONG>$t_author</STRONG></P>\n";
    } else { &write_warnings("There is no author for this document."); }
    if (($t_date)&&!($t_date=~/^\s*(($O|$OP)\d+($C|$CP))\s*\1\s*$/)) {
        $the_title .= "<BR><P ALIGN=CENTER><I>Revision: </I>$t_docrv; $t_date</P>\n";}
 
    $the_title . $_ ;
}

# -------------------------

sub bot_navigation_panel {
    local($_) = @_;
    local($bot_panel) = '';
    #  Start with a horizontal rule and descriptive comment
    $bot_panel .= "<HR>\n" . "<!--Navigation Panel-->" ;
    # Now add a few buttons, with a space between them
    $bot_panel .=   "$PREVIOUS $UP $NEXT $CONTENTS $INDEX $CUSTOM_BUTTONS" ; 
    # Line break    
    $bot_panel .= "\n<BR>" ;
    # ... and the ``previous'' title
    $bot_panel .= ($PREVIOUS_TITLE ? "\n<B> Previous:</B> $PREVIOUS_TITLE\n" : undef) ;
    # Similarly with the ``up'' title ...
    $bot_panel .= ($UP_TITLE ? "\n<B>Up:</B> $UP_TITLE\n" : undef) ;
    # If ``next'' section exists, add its title to the navigation panel
    $bot_panel .= ($NEXT_TITLE ? "\n<B> Next:</B> $NEXT_TITLE" : undef) ;
    # If ``index'' section exists, add its title to the navigation panel
    $bot_panel .= ($INDEX_TITLE ? "\n<B> Index:</B> $INDEX_TITLE" : undef) ;    
    # add "TOP" title to the navigation panel
    $bot_panel .= "\n<B> Top:</B> <a href=\"main.htm\">Main Page</a>" ;

    $bot_panel . $_ ;
}

sub top_navigation_panel {
    local($_) = @_;
    local($top_panel) = '';
    # change color
    $top_panel .= "<body text=\"\#000000\" bgcolor=\"\#FFFFFA\">\n" ;
    #  Start with a descriptive comment
    $top_panel .= "<!--Navigation Panel-->" ;
    # Now add a few buttons, with a space between them
    $top_panel .=   "$PREVIOUS $UP $NEXT $CONTENTS $INDEX $CUSTOM_BUTTONS" ; 
    # Line break    
    $top_panel .= "\n<BR>" ;
    # ... and the ``previous'' title
    $top_panel .= ($PREVIOUS_TITLE ? "\n<B> Previous:</B> $PREVIOUS_TITLE\n" : undef) ;
    # Similarly with the ``up'' title ...
    $top_panel .= ($UP_TITLE ? "\n<B>Up:</B> $UP_TITLE\n" : undef) ;
    # If ``next'' section exists, add its title to the navigation panel
    $top_panel .= ($NEXT_TITLE ? "\n<B> Next:</B> $NEXT_TITLE" : undef) ;
    # If ``index'' section exists, add its title to the navigation panel
    $top_panel .= ($INDEX_TITLE ? "\n<B> Index:</B> $INDEX_TITLE" : undef) ;    
    # add "TOP" title to the navigation panel
    $top_panel .= "\n<B> Top:</B> <a href=\"main.htm\">Main Page</a>";

    $top_panel . $_ ;
}

# ---------------------------

#  &process_commands_inline_in_tex (<<_RAW_ARG_INLINE_CMDS_);
#  thedocid
#  #messages # <<\\endmessages>>
#  _RAW_ARG_INLINE_CMDS_


#  &process_commands_nowrap_in_tex (<<_RAW_ARG_NOWRAP_CMDS_);
#  docid # {}
#  _RAW_ARG_NOWRAP_CMDS_

#  &process_commands_wrap_deferred (<<_RAW_ARG_WRAP_CMDS_);
#  docid # {}
#  thedocid
#  messages # <<\\endmessages>>
#  _RAW_ARG_WRAP_CMDS_

&ignore_commands( <<_IGNORED_CMDS_);
#setlength # {} # {}
hsize
#parbox # [] 
_IGNORED_CMDS_

1;                              # This must be the last line


