#!/perl
# LaTeX2HTML l2hconf.pm
# $Id: l2hconf.pin,v 1.17 2002/06/15 22:46:36 RRM Exp $

package main;

use vars qw(%used_icons);

# Setting this variable to where your perl executable resides can lead to
# better performance on some platforms.
#
# It is advisable to do this on an Intel system; e.g.
# $PERL='g:/usr/bin/perl_.exe';
#
# On a unix system it may be best left empty, or set as in:
# $PERL='/usr/local/bin/perl';
#
$PERL = '/usr/bin/perl';

# ############### THESE VARIABLES ARE DETERMINED BY CONFIGURE ################

#  Give the paths to latex and dvips on your system:
#
$LATEX = '/sw2/bin/latex';	# LaTeX
$DVIPS = '/sw2/bin/dvips';	# dvips


#  give these too, to use the  -ldump  feature
#
$TEX = "/sw2/bin/tex"; 			# TeX
$INILATEX = " \"&latex\"";	# initex+latex


# These affect whether images are made on a white or gray background.
# They are ignored when the document preamble contains similar commands.
# Use these defaults for dark solid (anti-aliased) characters in text and math.
#
$LOAD_LATEX_COLOR = "\\usepackage[dvips]{color}";
$LATEX_COLOR = "\\pagecolor[gray]{.7}";


# -white
# this overrides the above gray-scale for figures that don't need anti-aliasing
#
$WHITE_BACKGROUND = 0;


# -image_type
# This specifies the type of images produced by latex2html when processing
# unknown environments and/or e.g. math formulae.
#
@IMAGE_TYPES = qw(png gif);
$IMAGE_TYPE  = $IMAGE_TYPES[0];


# -tmp
# Specify a  tmp  directory for image-generation (optional)
#
$TMP = '/tmp';


#############  HTML validation  ###############
#
# set $HTML_VALIDATOR to the command needed to run a validator to check
# the HTML pages produced;
# use the  -validate  switch to run the validator, or set $HTML_VALIDATE
#
$HTML_VALIDATOR = '';


# -validate
# when $HTML_VALIDATE is 1, the validator will run as default
# provided $HTML_VALIDATOR is also set;  -novalidate  suppresses this
#
$HTML_VALIDATE = 0;


##########  ICONSERVER  --- !! IMPORTANT !! ############################
#
# LaTeX2HTML uses many small graphics as icons within the navigation
# panels, and for other purposes.
# You *must* specify where these graphics are to be found, for use within
# your documents.
#
# IMPORTANT: This location must not only be accessible to you, but also
#            to the people who are to read your HTML documents.
#
# One option is to always use the  -local_icons switch (see below)
# to have a copy of the icons together with your HTML document.
# (This is safe, but wasteful if you have a large number of documents.)
#
# A better option is to set  $ICONSERVER  to point to a location that
# is known to always (at least in principle) be available publicly.
#
# The default value for  $ICONSERVER  given below is within the local 
# $LATEX2HTMLDIR. If this is *not* to be publicly available then you 
#  **should change the value of $ICONSERVER to a site that is**.
#
# You probably have to talk to your Webmaster to provide access to the
# icon directory. Some hints:
# a) It's ok to set $ICONSERVER just to "/path/to/icons.$IMAGE_TYPE" (without
#    the internet address) if /path/to is valid for your file system, *and*
#    if www_root/path/to points to the same directory. Normally the www area
#    is located in some subsidary directory, which is pointed to by www_root
#    (the entry 'Document Root' of the http daemon's srm.conf file).
#    Make the Webmaster add appropriate links in that directory that help
#    locate the icon directory, or set up an Alias in srm.conf.
# b) To check if the icons can be displayed properly, invoke the browser
#    with the *http URL to your site*, and click down to your document.
# c) If you have $LOCAL_ICONS set, your document will accumulate a pretty
#    amount of redundant icons if you make use of segmentation.
#    In this case, customize &img_tag to use a central directory, say,
#    "../icons".
#
$ICONSERVER = ''||'file:/sw2/share/lib/latex2html/icons';
$ALTERNATIVE_ICONS = 0;


# ####### YOU *MAY* WANT/NEED TO CHANGE SOME OF THESE VARIABLES  ##############

# -djgpp
# On DOS/DJGPP systems one can easily run out of file handles. To
# avoid that, set this to 1. However this affects performance.
#
$DJGPP = 0;


#  if you are having difficulties with inputs not being found,
#  and your system is Web2C then setting this to 1 may help.
#
$Web2C = 1;


# Options for dvips as determined by configure
#
$DVIPSOPT = ' -Ppdf  -E';


# If you already have the fonts, you may add -M to suppress font
# generation
#
# $DVIPSOPT .= ' -M';


# If you have dvips 5.62 or higher, you can turn on generation of EPS files
# by uncommenting the following line. Warning: dvips does not support
# included EPS figures very well. However if you don't make use of
# complicated image include commands like \includegraphics, this option
# will speed up image generation a *lot*.
#
 $DVIPSOPT .= " -E";


# (Note: this here is old, don't worry unless you really run into trouble.)
#
# Some dvips programs generate postscript images in the reverse order by
# default. If your inlined images are all screwed up try uncommenting
# the following line:
#
# $DVIPSOPT .= " -r0";


# Modern TeX installations have PostScript Type 1 fonts which can be
# used instead of bitmaps. Use of these can give better quality images
# as Ghostscript can use `hinting' as well as having accurate outlines
# which help with anti-aliasing. Possible options here depend upon the
# TeX installation; e.g. " -Pcmz -Pams"  or " -Ppdf" 
#
# $DVIPSOPT .= " -Ppdf";


# For efficient use of font resources, minimising disk-space use,
# allow GhostScript to find the fonts it needs for images.
#
# DO:
#   EITHER:  edit Ghostscript's  $GS_LIB/Fontmap  file
#   OR:      set the GS_FONTPATH environment variable;
#   e.g.
#  $ENV{'GS_FONTPATH'} = join(':/usr/local/texmf/fonts/type1/', '',
#	'adobe','ams','bh','bsr','lucida','mt','public/cm','public/xypic');
#
# AND
#   setup a virtual printer configuration file  'config.gs' 
#   and listing  'psfonts.gs'  of PostScript fonts to exclude from .ps files
#
# AND
#   tell dvips to use this 'virtual printer' :
#   (the previous item for Type 1 fonts becomes redundant)
#
#  $DVIPSOPT .= " -Pgs";


# Local initialization files are usually named  .latex2html-init
# this name is hard-coded as the default with the latex2html script
# It can be changed here, if desired:
#
# $INIT_FILE_NAME = '.latex2html-init';


# Location of texexpand, supplied with the translator
#
$TEXEXPAND = "$PERL /sw2/bin${dd}texexpand";


# Location of pstoimg, supplied with the translator
#
$PSTOIMG = "$PERL /sw2/bin${dd}pstoimg";


# This is used to "autoload" perl code to deal with specific style files
#
$LATEX2HTMLSTYLES = "$LATEX2HTMLDIR${dd}styles";


# This is used to support upcoming versions of html - directory where perl
# files to handle those are
#
$LATEX2HTMLVERSIONS = "$LATEX2HTMLDIR${dd}versions";


# The following variable sets the default search list of directories for
# latex style files that latex2html should process.  It also defines a
# a list of directories (: separated) which possibly contain TeX and dvips
# inputs.  This variable is overriden by the environment variable
# TEXINPUTS, if it is specified.  Internally, the directory your document
# resides in, and ".", are appended to this list of directories.
#
#  IMPORTANT:  In some installations, latex and dvips are really
#  shell scripts which set environment TEXINPUTS (and other variables)
#  to predefined values, then call the real latex and dvips.  If this
#  is true for your installation, then the $TEXINPUTS that latex2html
#  sees will only affect the processing of \input and \include's
#  by latex2html, not the operation of latex and dvips when called
#  by latex2html.  In this case, make sure that the predefined
#  values of TEXINPUTS within the latex and dvips scripts at least
#  contains "." and ".." (".." works in the most cases).
#  Otherwise, latex and dvips will not find inputs from the original source
#  directory when called from a subdirectory contained therein.
#
# The single colon tells LaTeX to look on the standard places only.
# If you add entries, do it colon-separated.
# If you don't know where LaTeX takes its standard files from, leave the
# single colon in front or at the end, or have an empty entry "::"
# at some place among the other entries.
#
if ($Web2C) {
    chomp ($TEXINPUTS =
         # `kpsewhich -progname=latex -expand-braces \\\$TEXINPUTS`);
         #`kpsewhich -v -n latex \\\$TEXINPUTS`);
         $envkey);
} else {
    $TEXINPUTS = $envkey;
}


# This line helps LaTeX2HTML to recognize your adaption everywhere.
#
$ENV{'TEXINPUTS'} = $TEXINPUTS unless defined $ENV{'TEXINPUTS'};


# -no_fork
# If defined this will prevent the translator to crash if your operating
# system does not support forking; e.g. DOS.
#
$CAN_FORK = 1;


# ############################################################################
# THERE IS NO NEED TO CHANGE ANY OF THE VARIABLES BELOW EXCEPT FOR CUSTOMISING
# THE OPERATION OF LATEX2HTML.
# ############################################################################

### Command Line Argument Defaults #######################################

# -ldump
# Change this to 1 if you want to speed up image processing during the 2nd
# and more runs of LaTeX2HTML on the same document.
# This will cause LaTeX2HTML to produce a LaTeX dump of images.tex which
# is read in on subsequent runs and speeds up startup time of LaTeX on the
# images.tex translation.
# This actually consumes additional time on the first run, but pays off on
# subsequent runs. The dump file will need about 1 Meg of disk space.
#
$LATEX_DUMP = 0;


# -numbered_footnotes
# If defined to 1 you will get every footnote applied with a subsequent
# number, else with a hyperlink icon.
#
$NUMBERED_FOOTNOTES = 0;


# -local_icons
# Change this to 1 if you want to copy the navigation icons to each
# document directory so that the document directory is self-contained
# and can be dropped into another server tree. Note that you can also
# use the command line option -local_icons
#
$LOCAL_ICONS = 0;


# -split
#
$MAX_SPLIT_DEPTH = 8;	# Stop making separate files at this depth


# -link 
#
$MAX_LINK_DEPTH = 4;    # Stop showing child nodes at this depth   


# -short_extn
# If this is set all HTML file will have extension ".htm" instead of
# ".html". This is helpful when shipping the document to PC systems.
#
$SHORTEXTN = 0;


# -nolatex
#
$NOLATEX = 0;           # 1 = do not pass unknown environments to Latex


# -external_images
#
$EXTERNAL_IMAGES = 0;   # 1 = leave the images outside the document 


# -ps_images
# 1 =  use links to external postscript images rather than inlined GIF's.
#
$PS_IMAGES = 0;


# ANTI-ALIASING within generated images
#
# -antialias
# 1 =  use anti-aliasing in the generation of images of figures .
#
$ANTI_ALIAS = 0;


# -antialias_text
# 1 =  use anti-aliasing in the generation of images of typeset material;
#      e.g. mathematics and text, e.g. in tables and {makeimage} environments.
#
$ANTI_ALIAS_TEXT = 1;


# -font_size
# To set the point size of LaTeX-generated GIF files, uncomment the following
# variable, and set it to its desired value (i.e, 10pt, 11pt, 12pt, etc.)
# The default is to use the point size of the original LaTeX document.
# This value will be magnified by $FIGURE_SCALE_FACTOR and
# $MATH_SCALE_FACTOR (below).
#
# $FONT_SIZE = "12pt";


# -no_tex_defs
# To suppress the interpretation of raw TeX commands, set $TEXDEFS = 0;
# Note:  There are many variations of \def that latex2html cannot process
# correctly!
#
$TEXDEFS = 1;


# -ascii_mode
# This is different from -no_images.
# If this is set, LaTeX2HTML will show textual tags rather than
# images, both in navigation panel and text (Eg. [Up] instead the up
# icon).
# You could use this feature to create simple text from your
# document, eg. with 'Save as... Text' from Netscape or with
# lynx -dump.
#
$ASCII_MODE = 0;        # 1 = do not use any icons or internal images


# -t, The document title.
#
$default_title = '$FILE';


# -dir
$DESTDIR = '';         # Put the result in this directory 


# -no_subdir
# When this is set, the generated HTML files will be placed in the 
# current directory. If set to 0 the default behaviour is to create (or reuse)
# another file directory.
#
$NO_SUBDIR = 0;


# -address
# Supply your own string if you don't like the default <Name> <Date>
#
$ADDRESS = "<I>$address_data[0]</I>\n<BR><I>$address_data[1]</I>";


# -no_navigation
# 1 = do not put a navigation panel at the top of each page
#
$NO_NAVIGATION = 0;


# -top_navigation
# Determines whether to navigation links should be at the top or the bottom
# of each page. The default is at the top.
#
$TOP_NAVIGATION = 1;


# -bottom_navigation
# Determines whether to navigation links should be at the top or the bottom
# of each page. The default is at the top.
#
$BOTTOM_NAVIGATION = 0;


# -auto_navigation
# Put navigation links at the top of each  page.  If  the page  exceeds
# $WORDS_IN_PAGE  number of words then put one at the bottom of the page.
#
$AUTO_NAVIGATION = 1;


# -index_in_navigation
# Put a link to the index page in  the  navigation  panel
#
$INDEX_IN_NAVIGATION = 1;


# -contents_in_navigation
# Put a link to the table of contents  in  the  navigation  panel
#
$CONTENTS_IN_NAVIGATION = 1;


# -next_page_in_navigation
# Put a link to the next logical page  in  the  navigation  panel
#
$NEXT_PAGE_IN_NAVIGATION = 1;


# -previous_page_in_navigation
# Put a link to the previous logical page  in  the  navigation  panel
#
$PREVIOUS_PAGE_IN_NAVIGATION = 1;


# -prefix
# Set the output file prefix, prepended to all .html, .gif and .pl files.
# See also $AUTO_PREFIX.
#
$PREFIX = '';


# -auto_prefix
# To automatically insert the equivalent of "-prefix basename-", where
# "basename" is the base name of the file being translated, set this to 1.
#
$AUTO_PREFIX = 0;


# -up_url, -up_title, -down_url, -down_title, -prev_url, -prev_title:
# If both of the following two variables are set then the "Up" button
# of the navigation panel in the first node/page of a converted document
# will point to $EXTERNAL_UP_LINK. $EXTERNAL_UP_TITLE should be set
# to some text which describes this external link.
#
$EXTERNAL_UP_LINK  = '';
$EXTERNAL_UP_TITLE = '';


# Similarly you might set these variables to link external documents
# to your navigation panel.
#
$EXTERNAL_DOWN_LINK = "";
$EXTERNAL_DOWN_TITLE = "";
$EXTERNAL_PREV_LINK = "";
$EXTERNAL_PREV_TITLE = "";

$EXTERNAL_INDEX = "";
$EXTERNAL_CONTENTS = "";


# -info
# 0 = do not make an "About this document..." section 
#
$INFO = 1;


# -reuse, -no_reuse, Image recycling:
# If 0, do not reuse or recycle identical images. If the html subdirectory 
#	already exists, start the interactive session.
# If nonzero, do recycle them and switch off the interactive session.
# If 1, only recycle images generated from previous runs.
# If 2, recycle images from the current and previous runs.
#
$REUSE = 2;


# -no_images
# When $NO_IMAGES is set LaTeX2HTML will not attempt to produce any inlined images
# The missing images can be generated "off-line" by restarting LaTeX2HTML
# after setting $IMAGES_ONLY (see below);
$NO_IMAGES = 0;


# -images_only
# When $IMAGES_ONLY is set, LaTeX2HTML will only try to convert the inlined images
# in the file "images.tex" which should have been generated automatically during
# previous runs. This is very useful for correcting "bad LaTeX" in this file.
#
$IMAGES_ONLY = 0;


# -discard
# When $DISCARD_PS is set, the PostScript file created for each generated image
# is discarded immediately after its image has been rendered and saved in the
# required graphics format. This can lead to significant savings in disk-space,
# when there are a lot of images, since otherwise these files are not discarded 
# until the end of all processing.
#
$DISCARD_PS = 1;


# -show_section_numbers
# When this is 1, the section numbers are shown. The section numbers should 
# then match those that would have bee produced by LaTeX.
# The correct section numbers are obtained from the $FILE.aux file generated 
# by LaTeX.
# Hiding the seciton numbers encourages use of particular sections 
# as standalone documents. In this case the cross reference to a section 
# is shown using the default symbol rather than the section number.
#
$SHOW_SECTION_NUMBERS = 0;


# -short_index
# If this is set then  makeidx.perl  will construct codified names
# for the text of index references.
#
$SHORT_INDEX = 0;


# -debug
#  If this is set then intermediate files are left for later inspection.
#  This includes $$_images.tex and $$_images.log created during image
#  conversion.
#  Caution: Intermediate files can be *enormous*.
#
$DEBUG = 0;


# -html_version
# The default HTML version to be produced
#
$HTML_VERSION = '3.2';


# -no_math
# By default the special MATH extensions are not used
# since they do not conform with the HTML 3.2 standard.
#
$NO_SIMPLE_MATH = 1;


# -unsegment
# Use this to translate a segmented document as if it were not
# segmented.
#
$UNSEGMENT = 0;


### Other global variables ###############################################

# If this is set then the HTML will look better if viewed with Netscape.
#
$NETSCAPE_HTML = 0;


# Set this to 1 if you want interlaced images, 0 otherwise. Interlaced
# images build up gradually while downloading so one can get a first
# impression of what the final image will look like very quickly.
#
$INTERLACE = 1;


# Set this to 1 if you like the old LaTeX2HTML style to have a
# border around the navigation links.
# Values > 1 are also possible.
#
$NAV_BORDER = 0;
  

### Colors ###
#
# If this is set you may set colors in your document (see the LaTeX
# package color.dvi and the color.perl/colordvi.perl files).
# Note that HTML generated herefrom cannot be viewed by all browsers
# (at least Netscape or Mosaic 2.7 should do).
#
$COLOR_HTML = 0;

# Specify the path to your systems color database if you do not agree on
# the databases provided with the translator. Eg.: /usr/lib/X11/rgb.txt

# the RGB colors database
#
$RGBCOLORFILE = '/sw2/share/lib/latex2html/styles/rgb.txt';

# the CMYK colors database
#
$CRAYOLAFILE = '/sw2/share/lib/latex2html/styles/crayola.txt';

### End Colors ###

# Do not try to translate these input files, and do not
# complain about a missing Perl module.
# Complex LaTeX inputs, styles, or classes may cause the translator
# to hang. If this occurs add the input file here.
# You may also specify filename extensions here, e.g. if you do
# not want to include input files matching "*.myfig", add
# ``:.myfig''.
#
$DONT_INCLUDE = "2up:psfig:epsf:texinfo:pictex:" .
    ".ps:.eps:.fig:.pstex_t:.epsf:.epic:.eepic:.xy:.xya:.xyc:" .
    "titlepage:openbib:\\d+pt:twoside:twocolumn:" .
    "memo:dvipsfig:times:margins:aaii2:a4:art\\d+:doublespace:" .
    "alltt:amstex:anysize:array:article:bm:book:bookman:" .
    "boxedminipage:cite:comment:courier:dcolumn:doc:eepic:" .
    "enumerate:epic:fleqn:float:floatflt:fullpage:index:" .
    "inputenc:isolatin1:leqno:letter:llncs:makeidx:" .
    "multicol:psfig:report:shadow:shapepar:showidx:" .
    "slides:syntonly:sz:tabls:times:twoside:umlaut:umlaute";


# Latex2html usually does not include style files provided by
# \documentstyle, \documentclass, \usepackage but tries to use the 
# corresponding *.perl files provided in the styles/ subdirectory.
# Now if you use home-brew style files with new environments/commands
# you may want to include them. E.g. if you want to include "mystyle.sty",
# say $DO_INCLUDE = "mystyle" here. Separate styles with colons. This
# setting overrides the settings in $DONT_INCLUDE. You may specify
# filename extensions here as well.
#
# $DO_INCLUDE = "";


# If you have equations in your text, and must use bitmap'd fonts with 
# an old (pre 4.02) version of Ghostscript...
#  (RRM: ignore all the following with later versions)
#
#    ... you'll get the best results with PK_GENERATION=1.
# With this option switched on, DVIPS will be told to generate
# all of the images for a specific screen resolution,
# eliminating "blurring" of small letters and subscripts.
# If any of it causes you grief, simply set PK_GENERATION=0,
# and your default printer's resolution will be used.
#
# Sidik Isani, <isani@cfht.hawaii.edu> added this.
#
$PK_GENERATION = 0; # 0 # by configure


#    ... and set the following variable ONLY if your version of dvips
#  understands the "-mode" command line switch.  It is a more reliable
#  way of setting the METAfont mode than the .dvipsrc file on versions that
#  support this switch. If you do this, you do not need to modify .dvipsrc
#  as described below. Herb Swan <dprhws@edp.Arco.com> added this.
#
$DVIPS_MODE = '';


# Only if you have PK_GENERATION set to 1:
# A file 'modes.mf' is probably installed somewhere in your tex
# tree. e.g /usr/local/tex/texmf/mf/modes.mf or something similar.
# If it has and entry for 'toshiba', then you can ignore the rest.
# Otherwise, you'll need to try *one* of the following things:
#
#  o Set PK_GENERATION=0 and generate images for your default printer
#  o Download the latest modes.mf from the TeX archive and re-run inimf.
#  o Choose another LOW RESOLUTION screen or printer entry from modes.mf,
#    (preferably with "|blacker|" set to 0.0 and a 1:1 aspect ratio--
#     If you don't have "toshiba", try "epsonlq", "lqlores", "nec", or "NEC")
#     If you try some other mode, remember to update two other things as well:
#
#      1) Change the 'toshiba' in the .dvipsrc file that came with latex2html.
#      2) Put whatever the |pixels_per_inch| value is into $METAFONT_DPI
#         and the .dvipsrc file
#
#   Note: The maximum usable SCALE_FACTOR is determined by this resolution.
#   A higher resolution will take more memory during processing (regardless
#   of SCALE_FACTOR) but will allow higher SCALE_FACTORs.  Do NOT just
#   change this value though.  It *must* match the |pixels_per_inch| of
#   the metafont mode (e.g. `toshiba').
#
$METAFONT_DPI = 0;


# Controls which markup shows up between page and its foot.
$CHILDLINE = "<BR><HR>\n";


# If this is set as below, LaTeX2HTML produces a directory index link to
# the html document, ie. you may use <http://my.cite.is.here/dir/of/document>
# instead of <http://my.cite.is.here/dir/of/document/document.html>.
# Set it to eg. 'node1' to have the index pointing to node1.html, etc.
# Comment it out to have no index generation.
# Note: $EXTN is ".html" by default, see -short_extn.
#
$LINKPOINT = '"$FILE$EXTN"';

# Uses this one to determine the name of the directory index.
$LINKNAME = '"index$EXTN"';


# This is the line width measured in pixels and it is used to right justify
# equations and equation arrays; 
$LINE_WIDTH = 500;


# Used in conjunction with AUTO_NAVIGATION
$WORDS_IN_PAGE = 300;


# Affects ONLY the way accents are processed 
$default_language = 'english';	


# The value of this variable determines how many words to use in each 
# title that is added to the navigation panel (see below)
# 
#$WORDS_IN_NAVIGATION_PANEL_TITLES = 4;
$WORDS_IN_NAVIGATION_PANEL_TITLES = 6;


# This number will determine the size of the equations, special characters,
# and anything which will be converted into an inlined image
# *except* "image generating environments" such as "figure", "table" 
# or "minipage".
# Effective values are those greater than 0.
# Sensible values are between 0.1 - 4.
#
#$MATH_SCALE_FACTOR = 1.6;
$MATH_SCALE_FACTOR = 1.414;


# This number, when defined, determines extra scaling for displayed equations.
# It multiplies with the $MATH_SCALE_FACTOR to give the total scaling.
# It is especially useful when \scriptscriptstyle text is used frequently,
# which would otherwise be extremely difficult to read on-screen.
#
##$DISP_SCALE_FACTOR = 1;
#$DISP_SCALE_FACTOR = 1.0;


# This number will determine the size of 
# image generating environments such as "figure", "table" or "minipage".
# Effective values are those greater than 0.
# Sensible values are between 0.1 - 4.
#
$FIGURE_SCALE_FACTOR = 1.6;


# This is yet another scaling factor which has a special use.
# When this number is set, images are created at a size scaled by the
# specified amount (multiplying any other scale factors).
# However the images are displayed unscaled, by setting the
#  HEIGHT="..."  and  WIDTH="..." attributes to the unscaled size.
# Thus a larger image is squeezed into a smaller area.
# This allows for better quality when the HTML page is printed.
#  {figure}  environments are *not* affected by this factor.
#
##$EXTRA_IMAGE_SCALE = 2;
$EXTRA_IMAGE_SCALE = 1.415;


# If this is set to 0 then any inlined images generated from "figure" 
# environments will NOT be transparent.
#
$TRANSPARENT_FIGURES = 1;


# Set the default body text, inserted between <BODY> ... </BODY>.
# See also \bodytext{..} provided with html.sty.
#
$BODYTEXT = "";


# Valid paper sizes are "letter", "legal", "note" and ...
#   "a0", ... "a10", "b0", ... "b5";  
# Recommended:  "a5"
# Paper sizes has no effect other than with images that
# need special alignment; 
# e.g for equation-numbering with HTML, version 2.0
#  - larger paper sizes *MAY* help with large image problems 
#  - smaller paper sizes *MAY* be quicker to handle on some systems
#
$PAPERSIZE = "a5";

### Improved graphics support #################################################
# These utilities may be needed to implement some of the graphics effects
# that can be requested using optional parameters to LaTeX's \includegraphics
# command, from the  graphics.sty  and  graphicx.sty  packages.
# Thanks to Bruce Miller <bruce.miller@nist.gov> for revising support for
# these packages, via the module  styles/graphics-support.perl 

$PNMCUT = '/tmp/netpbm/bin/pnmcut';
$PNMFLIP = '/tmp/netpbm/bin/pnmflip';
$PNMPAD = '/tmp/netpbm/bin/pnmpad';
$PNMROTATE = '/tmp/netpbm/bin/pnmrotate';
$PNMSCALE = '/tmp/netpbm/bin/pnmscale';

$GIFTOPNM = '/tmp/netpbm/bin/giftopnm';
$JPEGTOPNM = '/tmp/netpbm/bin/jpegtopnm';
$PNGTOPNM = '/tmp/netpbm/bin/pngtopnm';
$PNMTOPNG = '/tmp/netpbm/bin/pnmtopng';
$PPMTOGIF = '/tmp/netpbm/bin/ppmtogif';
$PPMTOJPEG = '/tmp/netpbm/bin/ppmtojpeg';

# there are for some lesser-used (platform-specific ?) graphics formats:
$TIFFTOPNM = '/tmp/netpbm/bin/tifftopnm';
$ANYTOPNM = '/tmp/netpbm/bin/anytopnm';
$BMPTOPPM = '/tmp/netpbm/bin/bmptoppm';
$PCXTOPPM = '/tmp/netpbm/bin/pcxtoppm';
$PICTTOPPM = '/tmp/netpbm/bin/picttoppm';
$SGITOPNM = '/tmp/netpbm/bin/sgitopnm';
$XBMTOPBM = '/tmp/netpbm/bin/xbmtopbm';
$XWDTOPNM = '/tmp/netpbm/bin/xwdtopnm';

# uncomment these, and adjust  configure.in  to find the executable
# $FIASCOTOPNM = ;
# $FITSTOPNM = ;
# $GEMTOPNM = ;
# $JBIGTOPNM = ;
# $PALMTOPNM = ;
# $PAMTOPNM = ;
# $PSTOPNM = ;
# $RASTTOPNM = ;
# $RLETOPNM = ;
# $SIRTOPNM = ;
# $ZEISSTOPNM = ;

# $IMGTOPPM = ;
# $RGB3TOPPM = ;
# $TGATOPPM = ;
# $XIMTOPPM = ;
# $XPMTOPPM = ;
# $XVMINITOPPM = ;
# $XVPICTOPPM = ;
# $YUVTOPPM = ;

# $ICONTOPBM = ;
# $WBMTOPBM = ;
# $YBMTOPBM = ;

#
### Internationalization ######################################################
#
# Default values used by do_cmd_tableofcontents and others.
# Change them to suit your documents

sub english_titles {
    $toc_title = "Contents";
    $lof_title = "List of Figures";
    $lot_title = "List of Tables";
    $idx_title = "Index";
    $ref_title = "References";
    $bib_title = "Bibliography";
    $abs_title = "Abstract";
    $app_title = "Appendix";
    $pre_title = "Preface";
    $foot_title = "Footnotes";
    $thm_title = "Theorem";
    $fig_name = "Figure";
    $tab_name = "Table";
    $prf_name = "Proof";
    $date_name = "Date";
    $page_name = "Page";
  #  Sectioning-level titles
    $part_name = "Part";
    $chapter_name = "Chapter";
    $section_name = "Section";
    $subsection_name = "Subsection";
    $subsubsection_name = "Subsubsection";
    $paragraph_name = "Paragraph";
  #  Misc. strings
    $child_name = "Subsections";
    $info_title = "About this document ...";
    $also_name = "see also";
    $see_name = "see";
  #  names in navigation panels
    $next_name = "Next";
    $up_name = "Up";
    $prev_name = "Previous";
    $group_name = "Group";
  #  mail fields
    $encl_name = "encl";
    $headto_name = "To";
    $cc_name = "cc";

    @Month = ('', 'January', 'February', 'March', 'April', 'May',
	      'June', 'July', 'August', 'September', 'October',
	      'November', 'December');
# These words will be omitted from filenames derived
# from section-titles, when using  -long_titles
    $GENERIC_WORDS = "and|the|of|for|by|a|an|to";
}

# These words will be omitted from filenames derived
# from section-titles, when using  -long_titles
# Override this value within a  <language>_titles  subroutine.
#
$GENERIC_WORDS = "and|the|of|for|by|a|an|to";


# Replace "english" with another language provided
# titles for that language are defined, as above...
# (Make sure that you don't use a different default in your personal 
#  configuration file)
#
$TITLES_LANGUAGE = "english";


# ... or use titles in a different language by adding a new subroutine 
# eg for esperanto:
#   sub esperanto_titles {
#       $toc_title = 'Esperanto title';
#       etc...
#   }
# and then say 
# $TITLES_LANGUAGE = "esperanto";
#
# Note:  This is automatically done for you when use the german or
#	french style file, and for several other languages also,
#	or when you specify the language through the babel package.


### Verbosity #################################################################
#
# -verbosity
# The amount of message information printed to the screen during processing
# by LaTeX2HTML is controlled by the $VERBOSITY variable.
# Its value can also be set using the  -verbosity <num>  command-line switch.
# By increasing this value, more information is displayed.
# Here is the type of extra information that is shown at each level:
#
# $VERBOSITY = 0;	# no extra information
# $VERBOSITY = 1;	# section types and titles
# $VERBOSITY = 2;	# environment
# $VERBOSITY = 3;	# command names
# $VERBOSITY = 4;	# links, labels and internal sectioning codes
#
$VERBOSITY = 1;


### Navigation Panel ##########################################################
#
# The navigation panel is constructed out of buttons and section titles.
# These can be configured in any combination with arbitrary text and 
# HTML tags interspersed between them. 
# The buttons available are:
# $PREVIOUS - points to the previous section
# $UP  - points up to the "parent" section
# $NEXT - points to the next section
# $NEXT_GROUP - points to the next "group" section
# $PREVIOUS_GROUP - points to the previous "group" section
# $CONTENTS - points to the contents page if there is one
# $INDEX - points to the index page if there is one
#
# If the corresponding section exists the button will contain an
# active link to that section. If the corresponding section does
# not exist the button will be inactive.
#
# Also for each of the $PREVIOUS $UP $NEXT $NEXT_GROUP and $PREVIOUS_GROUP
# buttons there are equivalent $PREVIOUS_TITLE, $UP_TITLE, etc variables
# which contain the titles of their corresponding sections. 
# Each title is empty if there is no corresponding section.
#
# The subroutine below constructs the navigation panel in each page.
# Feel free to mix and match buttons, titles, your own text, your logos,
# and arbitrary HTML (the "." is the Perl concatenation operator).
#JKR: Use two panels (top and bot) instead of one.
#
# This is the default form of the navigation panel:

sub navigation_panel {
    "<!--Navigation Panel-->"

    # Now add a few buttons with a space between them
    . "$NEXT $UP $PREVIOUS $CONTENTS $INDEX $CUSTOM_BUTTONS"

    . "\n<BR>"		# Line break

    # If ``next'' section exists, add its title to the navigation panel
    . ($NEXT_TITLE ? "\n<B> $next_name:</B> $NEXT_TITLE" : undef)

    # Similarly with the ``up'' title ...
    . ($UP_TITLE ? "\n<B> $up_name:</B> $UP_TITLE" : undef)

    # ... and the ``previous'' title
    . ($PREVIOUS_TITLE ? "\n<B> $prev_name:</B> $PREVIOUS_TITLE" : undef)

    # ... and the ``contents'' title
    . ($CONTENTS_LINK ? "\n &nbsp; <B> $CONTENTS_LINK</B> " : undef)

    # ... and the ``index'' title
    . ($INDEX_LINK ? "\n &nbsp; <B> $INDEX_LINK</B> " : undef)

    # These <BR>s separate it from the text body.
    . "\n<BR><BR>"
}

# This can be redefined in an initialization file:
 if (!(defined &main::top_navigation_panel)) {
    eval "sub top_navigation_panel { \&navigation_panel(\@_) }"
 } else {
    print "\n *** top_navigation_panel subroutine already defined\n"
 }

sub bot_navigation_panel {

    #  Start with a horizontal rule (3-d dividing line)
    "<HR>\n" . "<!--Navigation Panel-->"

    # Now add a few buttons with a space between them
    . "$NEXT $UP $PREVIOUS $CONTENTS $INDEX $CUSTOM_BUTTONS"

    . "\n<BR>"		# Line break

    # If ``next'' section exists, add its title to the navigation panel
    . ($NEXT_TITLE ? "\n<B> $next_name:</B> $NEXT_TITLE" : undef)

    # Similarly with the ``up'' title ...
    . ($UP_TITLE ? "\n<B> $up_name:</B> $UP_TITLE" : undef)

    # ... and the ``previous'' title
    . ($PREVIOUS_TITLE ? "\n<B> $prev_name:</B> $PREVIOUS_TITLE" : undef)

    # ... and the ``contents'' title
    . ($CONTENTS_LINK ? "\n &nbsp; <B> $CONTENTS_LINK</B> " : undef)

    # ... and the ``index'' title
    . ($INDEX_LINK ? "\n &nbsp; <B> $INDEX_LINK</B> " : undef)
}

### Meta Information #####################################################
# 
# This information will be inserted in the HEAD of the generated
# HTML file. It can be used by automatic indexing scripts (eg
# site-index.pl at http://www.ai.mit.edu/tools/site-index.html) 
# You can change the description, keywords, etc. values.
#
sub meta_information {
    local($_) = @_;
    # Cannot have nested HTML tags...
    do { s/<[^>]*>//g;
	"<META NAME=\"description\" CONTENT=\"$_\">\n" .
	"<META NAME=\"keywords\" CONTENT=\"$FILE\">\n" .
	"<META NAME=\"resource-type\" CONTENT=\"document\">\n" .
	"<META NAME=\"distribution\" CONTENT=\"global\">\n"
    } if $_;
}

### Icons ################################################################

# Icon names and real icon files. 

foreach $typ (@IMAGE_TYPES) {
    %{"icons_$typ"} = (
	'cross_ref_visible_mark' ,"crossref.$typ",
	'anchor_mark' , '', # ,'&#160;',
	'anchor_invisible_mark' , '', # ,'&#160;', 
	'up_visible_mark' ,"up.$typ", 
	'next_visible_mark' ,"nx_grp.$typ", 
	'previous_visible_mark' ,"pv_grp.$typ",
	'next_page_visible_mark' ,"next.$typ",
	'previous_page_visible_mark' ,"prev.$typ",
	'contents_visible_mark' ,"contents.$typ",
	'index_visible_mark' ,"index.$typ",
	'footnote_mark' ,"footnote.$typ",
	'up_inactive_visible_mark' ,"up_g.$typ", 
	'next_inactive_visible_mark' ,"nx_grp_g.$typ", 
	'previous_inactive_visible_mark' ,"pv_grp_g.$typ",
	'next_page_inactive_visible_mark' ,"next_g.$typ",
	'previous_page_inactive_visible_mark' ,"prev_g.$typ",
	'change_begin_visible_mark',"ch_begin.$typ",
	'change_begin_right_visible_mark',"ch_beg_r.$typ",
	'change_end_visible_mark',"ch_end.$typ",
	'change_end_right_visible_mark',"ch_end_r.$typ",
	'change_delete_visible_mark',"ch_delet.$typ",
	'change_delete_right_visible_mark',"ch_del_r.$typ"
    )
};
if (!%icons) {
    %icons = %{"icons_$IMAGE_TYPE"};
}

if (!%iconsizes) {
    %iconsizes = (
	'up' ,'WIDTH="26" HEIGHT="24"',
	'next','WIDTH="37" HEIGHT="24"',
	'previous','WIDTH="63" HEIGHT="24"',
	'next_group' ,'WIDTH="81" HEIGHT="24"',
	'next_inactive' ,'WIDTH="81" HEIGHT="24"',
	'previous_group','WIDTH="107" HEIGHT="24"',
	'change_begin','WIDTH="104" HEIGHT="24"',
	'change_begin_right','WIDTH="104" HEIGHT="24" ALIGN="RIGHT"',
	'change_end','WIDTH="104" HEIGHT="24"',
	'change_end_right','WIDTH="104" HEIGHT="24" ALIGN="RIGHT"',
	'change_delete','WIDTH="109" HEIGHT="24"',
	'change_delete_right','WIDTH="109" HEIGHT="24" ALIGN="RIGHT"',
	'contents','WIDTH="65" HEIGHT="24"',
	'index','WIDTH="43" HEIGHT="24"',
	'image','WIDTH="48" HEIGHT="24"'
    ); 
}

$extern_image_mark = &extern_image_mark();

sub extern_image_mark {
    "[IMAGE $_[0]]";
}

sub img_tag {
    local($iconmark) = @_;
    local($icon) = $icons{$iconmark};
    local($alt);
    local($align) = " ALIGN=\"BOTTOM\" ";

    $alt = join ('|', "up", "next_group", "next_inactive", "previous_group"
		, "next", "previous", "change_begin_right", "change_begin"
		, "change_end_right", "change_end", "change_delete_right"
		, "change_delete", "contents", "index");

    if ($icon =~ /(gif|png)$/) {
	$used_icons{$icon} = 1;
	if ($iconmark =~ /change_(begin|end|delete)_right/) { $align = ' ' };
	local($pre);
	local($nav_border) = "\"$NAV_BORDER\"";
	if ($iconmark =~ /($alt)/) {
	    $pre = "\n";
	    $alt = $1;
	}
	else {
	    $pre = "";
	    $nav_border = '"1"';
	    $alt = '[*]';
	 };

	if ($LOCAL_ICONS) {
	    return join('', $pre ,'<IMG ', $iconsizes{$alt} || '', $align
			,'BORDER=', $nav_border, ' ALT="', $alt
			,'" SRC="', $icon, '">' );
	}
	else {
	    return join('', $pre ,'<IMG ', $iconsizes{$alt} || '', $align
			,'BORDER=', $nav_border, ' ALT="', $alt, "\"\n"
			,' SRC="', $ICONSERVER, "/$icon", '">' );
	}
    }
    else {
	return $icon;
    }
}

sub inactive_img { 
    # Replaces an image name xxx.gif with xxx_gr.gif
    # It is assumed that _gr images contain the equivalent inactive icons
    local($_) = @_;
    s/(up|next|previous|next_page|previous_page)(_visible_mark)/$1_inactive$2/;
    $_;
}

### ASCII Mode ###########################################################

# This subroutine defines the ascii strings to be used instead of the 
# icons when the translator is invoked with the -ascii_mode option.
# Please modify them if you do not like them, BUT 
# *** DO NOT USE THE SPACE CHARACTER (" ") FOR $anchor_invisible_mark ***
# (if you use " " then the cross-reference hyperlinks will not work).
# --- this depends on the browser. From HTML 3.2, a space works OK now.

sub ascii_mode {
	$cross_ref_visible_mark = "[*]";
	$anchor_mark = "&#160;";	
	$anchor_invisible_mark = "&#160;"; 
	$up_visible_mark = "[$up_name]"; 
	$next_visible_mark = "[$next_name $group_name]"; 
	$previous_visible_mark = "[$prev_name $group_name]";
	$next_page_visible_mark ="[$next_name]";
	$previous_page_visible_mark ="[$prev_name]";
	$up_inactive_visible_mark  = "[$up_name]"; 
	$next_inactive_visible_mark = "[$next_name $group_name]"; 
	$previous_inactive_visible_mark = "[$prev_name $group_name]";
	$next_page_inactive_visible_mark ="[$next_name]";
	$previous_page_inactive_visible_mark ="[$prev_name]";
	$contents_visible_mark = "[$toc_title]";
	$index_visible_mark = "[$idx_title]";
	$footnote_mark = "[+]";
	$extern_image_mark = &extern_image_mark;
	$EXTERNAL_IMAGES = 1;
}

### Adding commands to be ignored ########################################

# Add LaTeX commands to be ignored.
# Each command should be on a separate line and have the format:
#  <cmd_name>#{}# []# {}# [] etc. 
# {} marks a compulsory argument and [] an  optional one.
# Note that some commands may have arguments which should be left as
# text even though the command should be ignored (e.g. mbox, center, etc)
#
&::ignore_commands( <<_IGNORED_CMDS_);
htmlrule # [] # \$_ = join('',"<BR><HR>",\$_) 
mathversion # {} 
underline # {} # \$_ = join('',"<U>", \$2, "</U>", \$_)
centerline # {} # \$_ = join('',"<P ALIGN=CENTER>", \$2, "</P>", \$_)
latexhtml# {}
latex# {}
html
lrule # {} 
scrollmode
savebox# {}# []# [] 
center
citeindexfalse
_IGNORED_CMDS_


### Adding commands to be processed by TeX ###############################

# Commands which need to be passed, ALONG WITH THEIR ARGUMENTS, to TeX.
# The syntax is the same as that for ignore_commands above.

&::process_commands_in_tex (<<_RAW_ARG_CMDS_);
fbox # {}
framebox # [] # [] # {}
_RAW_ARG_CMDS_
	
1;	# This must be the last line

