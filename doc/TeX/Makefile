# -*- Makefile -*-
# for a better html result, do 'make pdf' right begore 'make html' (so that the aux file of the former can be used by the latter)
TEX	:= latex
DVIPS   := dvips
PDFTEX  := pdflatex
TEXFLAGS := 
DVIFLAGS :=
PDFFLAGS :=
LATEX2HTML := latex2html
# LATEX2HTML := ~/bin/latex2html
VERSION =  $(shell grep newcommand hpxversion.tex | grep hpxversion | sed "s/[a-z{}\\]*//g")
HTMLDIR := ../../healpix-dynamicPages/html/
ADDRESS := -address "Version $(VERSION), `date +%Y-%m-%d`" -info 1
#L2HFLAGS := -dir $(HTMLDIR) -up_url main.htm -up_title "Main Page" -math -html_version 4.0 -bottom_navigation -short_extn -white -local_icons -noexternal_images -toc_depth 8 $(ADDRESS) # -debug -verbosity 2  -show_section_numbers 
L2HFLAGS := -dir $(HTMLDIR) -up_url main.htm -up_title "Main Page" \
	-down_url main.htm -down_title "Main Page" \
	-math -html_version 4.1 -bottom_navigation -short_extn -white -local_icons \
	-noexternal_images -toc_depth 8 $(ADDRESS) # -debug -verbosity 2 # -show_section_numbers 
EQNFIX := ./eqnfix.sh
CSSFILE := common.css
COLORS = 1

# put last the ones referring to the others
docs  := intro.tex csub.tex idl.tex subroutines.tex facilities.tex install.tex
style := healpix.sty # healrut.sty
dvi   := $(docs:.tex=.dvi)
ps    := $(dvi:.dvi=.ps)
gz    := $(dvi:.dvi=.ps.gz)
pdf   := $(docs:.tex=.pdf)   pdf_index.pdf
cross := $(docs:.tex=.cross) pdf_index.cross
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

# all PDF
pdf: $(pdf)
	cp $(pdf) ../../healpix-dynamicPages/pdf
	@echo "$(RED_COLOR)Type   make crossref   to get the cross-references right$(NO_COLOR)"

# reprocess all PDF to get cross-ref right
crossref: $(pdf) $(cross)
	cp $(pdf) ../../healpix-dynamicPages/pdf

# all HTML
html: $(html) main.html

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

# cross-reference pass on PDF
$(cross): %.cross : %.tex healpix.sty $(incs)
	@echo "$(GREEN_COLOR)regenerating    $*.pdf    for cross-referencing$(NO_COLOR)"
	-$(PDFTEX) $(PDFLLAGS) $< >/dev/null
	-$(PDFTEX) $(PDFLLAGS) $< >/dev/null

.PHONY: main.html

cssfile: $(CSSFILE)
	@echo ' copying CSS file '
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
	@echo "created$(BLUE_COLOR) $(HTMLDIR)main.htm $(NO_COLOR)"

install.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)        INSTALL.HTML              $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	mkdir -p $(HTMLDIR)
	-cp $(HTMLDIR)/*labels.pl /tmp
	-$(LATEX2HTML) $(L2HFLAGS) -split 3 -prefix install   -t "Installing HEALPix"      install
	-$(EQNFIX) $(HTMLDIR)/install
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_install
	ln -sf $(CSSFILE) $(HTMLDIR)/install.css
	@echo "created$(BLUE_COLOR) $(HTMLDIR)install.htm $(NO_COLOR)"

intro.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)          INTRO.HTML              $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	mkdir -p $(HTMLDIR)
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix intro -external_file ./intro    -t "Introduction to HEALPix"  intro
	-$(EQNFIX) $(HTMLDIR)/intro > /dev/null
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_intro
	ln -sf $(CSSFILE) $(HTMLDIR)/intro.css
	@echo "created$(BLUE_COLOR) $(HTMLDIR)intro.htm $(NO_COLOR)"


idl.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)            IDL.HTML              $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	mkdir -p $(HTMLDIR)
	-cp $(HTMLDIR)/*labels.pl /tmp
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix idl       -t "HEALPix/IDL subroutines"   idl
	-$(EQNFIX) $(HTMLDIR)/idl
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_idl
	ln -sf $(CSSFILE) $(HTMLDIR)/idl.css
	@echo "created$(BLUE_COLOR) $(HTMLDIR)idl.htm $(NO_COLOR)"


subroutines.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)      SUBROUTINES.HTML            $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	mkdir -p $(HTMLDIR)
	-cp $(HTMLDIR)/*labels.pl /tmp
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix subroutines -t "HEALPix/F90 subroutines" subroutines
	-$(EQNFIX) $(HTMLDIR)/subroutines
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_subroutines
	ln -sf $(CSSFILE) $(HTMLDIR)/subroutines.css
	@echo "created$(BLUE_COLOR) $(HTMLDIR)subroutines.htm $(NO_COLOR)"


facilities.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)        FACILITIES.HTML           $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	mkdir -p $(HTMLDIR)
	-cp $(HTMLDIR)/*labels.pl /tmp
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix facilities -t "HEALPix/F90 facilities"  facilities
	-$(EQNFIX) $(HTMLDIR)/facilities
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_facilities
	ln -sf $(CSSFILE) $(HTMLDIR)/facilities.css
	@echo "created$(BLUE_COLOR) $(HTMLDIR)facilities.htm $(NO_COLOR)"


csub.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)            CSUB.HTML             $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	mkdir -p $(HTMLDIR)
	-$(LATEX2HTML) $(L2HFLAGS) -prefix csub       -t "HEALPix/C subroutines"  csub
	-$(EQNFIX) $(HTMLDIR)/csub
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_csub
	ln -sf $(CSSFILE) $(HTMLDIR)/csub.css
	@echo "created$(BLUE_COLOR) $(HTMLDIR)csub.htm $(NO_COLOR)"


clean: 
	$(RM) *.aux *.log *.toc *.blg *.bbl *.lof *.lot *.kf *.out ; \
	$(RM) *~ "#*"

tidy: clean
	$(RM) *.dvi

nops: tidy
	$(RM) $(ps)

nopdf: tidy
	$(RM) $(pdf)

empty: nops nopdf
	$(RM) -r $(HTMLDIR)


