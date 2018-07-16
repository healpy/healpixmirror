# -*- Makefile -*-
# for a better html result, do 'make pdf' right before 'make html' (so that the aux file of the former can be used by the latter)
TEX	:= latex
DVIPS   := dvips
PDFTEX  := pdflatex
TEXFLAGS := 
DVIFLAGS :=
PDFFLAGS :=
LATEX2HTML := export L2HCONFIG=./my_l2hconf.pm ; export PATH=$(PATH):/usr/local/bin ; ./latex2html
#LATEX2HTML := export L2HCONFIG=./my_l2hconf.pm ; export PATH=$(PATH):/sw2/bin ; ./latex2html
#LATEX2HTML := export L2HCONFIG=./my_l2hconf_test.pm ; export PATH=$(PATH):/tmp/netpbm/bin ; ./latex2html
# LATEX2HTML := ~/bin/latex2html
#VERSION =  $(shell grep newcommand hpxversion.tex | grep hpxversion | sed "s/[a-z{}\\]*//g")
VERSION =  $(shell grep "^\\\\newcommand" hpxversion.tex | grep hpxversion | sed "s/[a-z{}\\]*//g")
DOCDIR  := ../../healpix-dynamicPages
HTMLDIR := $(DOCDIR)/html
PDFDIR  := $(DOCDIR)/pdf
ADDRESS := -address "Version $(VERSION), `date +%Y-%m-%d`" -info 1
#L2HFLAGS := -dir $(HTMLDIR) -up_url main.htm -up_title "Main Page" -math -html_version 4.0 -bottom_navigation -short_extn -white -local_icons -noexternal_images -toc_depth 8 $(ADDRESS) # -debug -verbosity 2  -show_section_numbers 
L2HFLAGS := -dir $(HTMLDIR) -up_url main.htm -up_title "Main Page" \
	-down_url main.htm -down_title "Main Page" \
	-math -html_version 4.1 -antialias \
	-bottom_navigation -short_extn -white -local_icons \
	-noexternal_images -toc_depth 8 $(ADDRESS) # -debug -verbosity 2 # -show_section_numbers 
#-math -html_version 4.1 
#-short_index -long_titles 6 
#-show_section_numbers -short_index 

EQNFIX      := ./eqnfix.sh
IMPROVEHTML := ./improve_mobile_html.sh
SHORTEN     := ./shorten_section_tag.sh
INSERTFAVICONS := ./insert_favicons.sh
CSSFILE     := common.css
COLORS = 1

# put last the ones referring to the others
docs  := intro.tex csub.tex idl.tex subroutines.tex facilities.tex install.tex
style := healpix.sty
dvi   := $(docs:.tex=.dvi)
ps    := $(dvi:.dvi=.ps)
gz    := $(dvi:.dvi=.ps.gz)
pdf   := $(docs:.tex=.pdf)   pdf_index.pdf
cross := $(docs:.tex=.cross) pdf_index.cross
chtml := $(docs:.tex=.chtml)
vpdf  := $(docs:.tex=.vpdf)
html  := $(docs:.tex=.html)
incs  := hpxversion.tex idlversion.tex gdlversion.tex hpxwebsite.tex healpix_src_url.tex

# define color output if needed
ifeq ($(COLORS),1)
NO_COLOR=\x1b[0m
GREEN_COLOR=\x1b[32;11m
RED_COLOR=\x1b[31;01m
BLUE_COLOR=\x1b[34;11m
endif

#----------------------------------

default:
	@echo "Welcome on HEALPix documentation Makefile"
	@echo "Valid options include:  "
	@echo " package"
	@echo "   all"
	@echo "     allpdf, crossref"
	@echo "       pdf"
	@echo "     crosshtml"
	@echo "       html"
	@echo "     empty"
	@echo "       tidy, nopdf, clean"

#----------------------------------
# all PDF
pdf: $(pdf)
	@echo "$(RED_COLOR)Type   make crossref   to get the cross-references right$(NO_COLOR)"
	mkdir -p $(PDFDIR)
	cp $(pdf) $(PDFDIR)

# reprocess all PDF to get cross-ref right
crossref: $(pdf) $(cross)
	mkdir -p $(PDFDIR)
	cp $(pdf) $(PDFDIR)
	@echo "$(GREEN_COLOR)looking for missing (cross)-references:$(NO_COLOR)"
	grep -E reference\ \|name\{ *.log

# cross-reference pass on each PDF
$(cross): %.cross : %.tex healpix.sty $(incs)
	@echo "$(GREEN_COLOR)regenerating    $*.pdf    for cross-referencing$(NO_COLOR)"
	-$(PDFTEX) $(PDFLLAGS) $< >/dev/null
	-$(PDFTEX) $(PDFLLAGS) $< >/dev/null

# force individual PDF (re)creation and open it
$(vpdf): %.vpdf : %.pdf
	\rm -f $<
	make $<
	open $<

# individual PDF
$(pdf): %.pdf : %.tex healpix.sty $(incs)
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)              $@                  $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	-$(PDFTEX) $(PDFLLAGS) $< 
	-$(PDFTEX) $(PDFLLAGS) $< >/dev/null
	-$(PDFTEX) $(PDFLLAGS) $< >/dev/null
	-$(PDFTEX) $(PDFLLAGS) $< >/dev/null
	@echo "$(GREEN_COLOR)          $@  done                $(NO_COLOR)\n"

#----------------------------------

.PHONY: main.html

#----------------------------------
# all HTML
html: $(html) main.html

# reprocess all HTML to get cross-ref right
crosshtml: $(html) $(chtml) main.html

# cross-reference pass on each HTML
$(chtml): %.chtml : %.html
	@echo "$(GREEN_COLOR)regenerating    $*.html    for cross-referencing$(NO_COLOR)"
	@make $<  1> /dev/null 2>&1

cssfile: $(CSSFILE)
	@echo ' copying CSS file '
	mkdir -p $(HTMLDIR)
	cp -f $(CSSFILE) $(HTMLDIR)

main.html: 
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)        MAIN.HTM                  $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	mkdir -p $(HTMLDIR)
	cp main.html $(HTMLDIR)/main.htm
	rm -f $(HTMLDIR)/index.htm
	cp main.html $(HTMLDIR)/index.htm
	cp main.html $(HTMLDIR)/index.html
#	cp fig/backsmall3balls.gif $(HTMLDIR)
	@echo "created$(BLUE_COLOR) $(HTMLDIR)/main.htm $(NO_COLOR)"

install.html: $(style) healpix.perl cssfile 
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)        INSTALL.HTML              $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	$(eval FULL   = install)
	$(eval PREFIX = ${FULL})
	$(eval LONGTITLES = )
	$(eval TITLE  = "Installing HEALPix")
	$(eval TTL    =  Installing_HEALPix)
	mkdir -p $(HTMLDIR)
	-cp $(HTMLDIR)/*labels.pl /tmp
	-$(LATEX2HTML) $(L2HFLAGS) -split 3 -prefix $(FULL) -external_file ./$(FULL)  -t $(TITLE) $(LONGTITLES)    $(FULL)
	-$(EQNFIX)      $(HTMLDIR)/$(PREFIX) 1> /dev/null 2>&1
	-$(SHORTEN)	$(HTMLDIR)/$(PREFIX)
	-$(IMPROVEHTML) $(HTMLDIR)/$(PREFIX)
	-$(INSERTFAVICONS) $(HTMLDIR)/$(PREFIX)
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_$(FULL)
	ln -sf $(CSSFILE)          $(HTMLDIR)/$(FULL).css
	#ln -sf $(PREFIX)$(TTL).htm $(HTMLDIR)/$(FULL).htm
	-cp $(HTMLDIR)/*labels.pl /tmp
	@echo "created$(BLUE_COLOR) $(HTMLDIR)/$(FULL).htm $(NO_COLOR)"

intro.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)          INTRO.HTML              $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	$(eval FULL   = intro)
	$(eval PREFIX = intro_)
	$(eval LONGTITLES = -long_titles 6)
	$(eval TITLE  = "Introduction to HEALPix")
	$(eval TTL    =  Introduction_HEALPix)
	mkdir -p $(HTMLDIR)
	-cp $(HTMLDIR)/*labels.pl /tmp
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix $(PREFIX) -external_file ./$(FULL) -t $(TITLE) $(LONGTITLES) $(FULL)
	-$(EQNFIX)      $(HTMLDIR)/$(PREFIX) 1> /dev/null 2>&1
	-$(SHORTEN)	$(HTMLDIR)/$(PREFIX)
	-$(IMPROVEHTML) $(HTMLDIR)/$(PREFIX)
	-$(INSERTFAVICONS) $(HTMLDIR)/$(PREFIX)
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_$(FULL)
	ln -sf $(CSSFILE)          $(HTMLDIR)/$(FULL).css
	ln -sf $(PREFIX)$(TTL).htm $(HTMLDIR)/$(FULL).htm
	-cp $(HTMLDIR)/*labels.pl /tmp
	@echo "created$(BLUE_COLOR) $(HTMLDIR)/$(FULL).htm $(NO_COLOR)"


idl.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)            IDL.HTML              $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	$(eval FULL   = idl)
	$(eval PREFIX = idl_)
	$(eval LONGTITLES = -long_titles 6)
	$(eval TITLE  = "HEALPix/IDL subroutines")
	$(eval TTL    =  HEALPix_IDL_subroutines)
	mkdir -p $(HTMLDIR)
	-cp $(HTMLDIR)/*labels.pl /tmp
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix $(PREFIX) -external_file ./$(FULL) -t $(TITLE) $(LONGTITLES) $(FULL)
	-$(EQNFIX)      $(HTMLDIR)/$(PREFIX) 1> /dev/null 2>&1
	-$(SHORTEN)	$(HTMLDIR)/$(PREFIX)
	-$(IMPROVEHTML) $(HTMLDIR)/$(PREFIX)
	-$(INSERTFAVICONS) $(HTMLDIR)/$(PREFIX)
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_$(FULL)
	ln -sf $(CSSFILE)          $(HTMLDIR)/$(FULL).css
	ln -sf $(PREFIX)$(TTL).htm $(HTMLDIR)/$(FULL).htm
	-cp $(HTMLDIR)/*labels.pl /tmp
	@echo "created$(BLUE_COLOR) $(HTMLDIR)/$(FULL).htm $(NO_COLOR)"


subroutines.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)      SUBROUTINES.HTML            $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	$(eval FULL   = subroutines)
	$(eval PREFIX = sub_)
	$(eval LONGTITLES = -long_titles 6)
	$(eval TITLE  = "HEALPix/F90 subroutines")
	$(eval TTL    =  HEALPix_F90_subroutines)
	mkdir -p $(HTMLDIR)
	-cp $(HTMLDIR)/*labels.pl /tmp
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix $(PREFIX) -external_file ./$(FULL) -t $(TITLE) $(LONGTITLES) $(FULL)
	-$(EQNFIX)      $(HTMLDIR)/$(PREFIX) 1> /dev/null 2>&1
	-$(SHORTEN)	$(HTMLDIR)/$(PREFIX)
	-$(IMPROVEHTML) $(HTMLDIR)/$(PREFIX)
	-$(INSERTFAVICONS) $(HTMLDIR)/$(PREFIX)
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_$(FULL)
	ln -sf $(CSSFILE)          $(HTMLDIR)/$(FULL).css
	ln -sf $(PREFIX)$(TTL).htm $(HTMLDIR)/$(FULL).htm
	-cp $(HTMLDIR)/*labels.pl /tmp
	@echo "created$(BLUE_COLOR) $(HTMLDIR)/$(FULL).htm $(NO_COLOR)"


facilities.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)        FACILITIES.HTML           $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	$(eval FULL   = facilities)
	$(eval PREFIX = fac_)
	$(eval LONGTITLES = -long_titles 6)
	$(eval TITLE  = "HEALPix/F90 facilities")
	$(eval TTL    =  HEALPix_F90_facilities)
	mkdir -p $(HTMLDIR)
	-cp $(HTMLDIR)/*labels.pl /tmp
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix $(PREFIX) -external_file ./$(FULL) -t $(TITLE) $(LONGTITLES) $(FULL)
	-$(EQNFIX)      $(HTMLDIR)/$(PREFIX) 1> /dev/null 2>&1
	-$(SHORTEN)	$(HTMLDIR)/$(PREFIX)
	-$(IMPROVEHTML) $(HTMLDIR)/$(PREFIX)
	-$(INSERTFAVICONS) $(HTMLDIR)/$(PREFIX)
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_$(FULL)
	ln -sf $(CSSFILE)          $(HTMLDIR)/$(FULL).css
	ln -sf $(PREFIX)$(TTL).htm $(HTMLDIR)/$(FULL).htm
	-cp $(HTMLDIR)/*labels.pl /tmp
	@echo "created$(BLUE_COLOR) $(HTMLDIR)/$(FULL).htm $(NO_COLOR)"


csub.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)            CSUB.HTML             $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	$(eval FULL   = csub)
	$(eval PREFIX = csub_)
	$(eval LONGTITLES = -long_titles 6)
	$(eval TITLE  = "HEALPix/C subroutines")
	$(eval TTL    =  HEALPix_C_subroutines)
	mkdir -p $(HTMLDIR)
	-cp $(HTMLDIR)/*labels.pl /tmp
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix $(PREFIX) -external_file ./$(FULL) -t $(TITLE) $(LONGTITLES) $(FULL)
	-$(EQNFIX)      $(HTMLDIR)/$(PREFIX) 1> /dev/null 2>&1
	-$(SHORTEN)	$(HTMLDIR)/$(PREFIX)
	-$(IMPROVEHTML) $(HTMLDIR)/$(PREFIX)
	-$(INSERTFAVICONS) $(HTMLDIR)/$(PREFIX)
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_$(FULL)
	ln -sf $(CSSFILE)          $(HTMLDIR)/$(FULL).css
	ln -sf $(PREFIX)$(TTL).htm $(HTMLDIR)/$(FULL).htm
	-cp $(HTMLDIR)/*labels.pl /tmp
	@echo "created$(BLUE_COLOR) $(HTMLDIR)/$(FULL).htm $(NO_COLOR)"

#----------------------------------

t1:	
	@export PATH=${OLDPATH}:${MAGICPATH} ; echo $$PATH ; ppmquant
	#ppmquant
	@echo $$PATH

clean: 
	$(RM) *.aux *.log *.toc *.blg *.bbl *.lof *.lot *.kf *.out ; \
	$(RM) *~ "#*" ; \
	$(RM) /tmp/*labels.pl

tidy: clean
	$(RM) *.dvi

nops: tidy
	$(RM) $(ps)

nopdf: tidy
	$(RM) $(pdf)

empty: nops nopdf
	$(RM) -r $(HTMLDIR)


allpdf: empty crossref
	@echo "$(GREEN_COLOR)           All PDF Done !         $(NO_COLOR)"

all: empty crossref crosshtml
	@echo "$(GREEN_COLOR)        All PDF+HTML Done !      $(NO_COLOR)"

package: all
	cd $(DOCDIR) ; \
	tar -czpf doc_$(VERSION).tar.gz pdf html ; \
	pwd
	@echo "$(GREEN_COLOR)  $(DOCDIR)/doc_$(VERSION).tar.gz  written   $(NO_COLOR)"


