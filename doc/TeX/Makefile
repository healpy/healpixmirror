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
HTMLDIR := ../../healpix-dynamicPages/html/
L2HFLAGS := -dir $(HTMLDIR) -up_url main.htm -up_title "Main Page" -math -html_version 4.0 -bottom_navigation -short_extn -white -local_icons -noexternal_images -toc_depth 8 # -debug -verbosity 2  -show_section_numbers 
#L2HFLAGS := -dir $(HTMLDIR) -up_url main.htm -up_title "Main Page" -math -html_version 4.1 -bottom_navigation -short_extn -white -local_icons -noexternal_images -toc_depth 8 -debug -verbosity 2 # -show_section_numbers 
EQNFIX := ./eqnfix.sh
CSSFILE := common.css

docs  := intro.tex facilities.tex subroutines.tex idl.tex csub.tex install.tex # install.* must be last
style := healpix.sty # healrut.sty
dvi   := $(docs:.tex=.dvi)
ps    := $(dvi:.dvi=.ps)
gz    := $(dvi:.dvi=.ps.gz)
pdf   := $(docs:.tex=.pdf) pdf_index.pdf
html  := $(docs:.tex=.html)

# dvi: $(dvi) 

# ps: $(ps)

pdf: $(pdf)
	cp $(pdf) ../../healpix-dynamicPages/pdf

# gz: $(ps)
# 	gzip -9vf $(ps)

html: $(html) main.html

$(pdf): %.pdf : %.tex healpix.sty hpxversion.tex idlversion.tex
	-$(PDFTEX) $(PDFLLAGS) $< 
	-$(PDFTEX) $(PDFLLAGS) $< >/dev/null
	-$(PDFTEX) $(PDFLLAGS) $< >/dev/null
	-$(PDFTEX) $(PDFLLAGS) $< >/dev/null

$(dvi): %.dvi : %.tex healpix.sty # healrut.sty
	-$(TEX) $(TEXFLAGS) $< 
	-$(TEX) $(TEXFLAGS) $< >/dev/null
	-$(TEX) $(TEXFLAGS) $< >/dev/null

$(ps): %.ps : %.dvi
	$(DVIPS) $(DVIFLAGS) -o $@ $<

.PHONY: main.html

cssfile: $(CSSFILE)
	@echo ' copying CSS file '
	cp -f $(CSSFILE) $(HTMLDIR)

main.html: 
	@echo '=================================='
	@echo '        MAIN.HTM                  '
	@echo '=================================='
	mkdir -p $(HTMLDIR)
	cp main.html $(HTMLDIR)/main.htm
	rm -f $(HTMLDIR)/index.htm
	cp main.html $(HTMLDIR)/index.htm
	cp main.html $(HTMLDIR)/index.html
#	cp fig/backsmall3balls.gif $(HTMLDIR)

install.html: $(style) healpix.perl cssfile
	@echo '=================================='
	@echo '        INSTALL.HTML              '
	@echo '=================================='
	mkdir -p $(HTMLDIR)
	-cp $(HTMLDIR)/*labels.pl /tmp
	-$(LATEX2HTML) $(L2HFLAGS) -split 3 -prefix install   -t "Installing HEALPix"       install
	-$(EQNFIX) $(HTMLDIR)/install
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_install
	ln -sf $(CSSFILE) $(HTMLDIR)/install.css

intro.html: $(style) healpix.perl cssfile
	@echo '=================================='
	@echo '          INTRO.HTML              '
	@echo '=================================='
	mkdir -p $(HTMLDIR)
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix intro -external_file ./intro    -t "Introduction to HEALPix"  intro
	-$(EQNFIX) $(HTMLDIR)/intro > /dev/null
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_intro
	ln -sf $(CSSFILE) $(HTMLDIR)/intro.css

idl.html: $(style) healpix.perl cssfile
	@echo '=================================='
	@echo '            IDL.HTML              '
	@echo '=================================='
	mkdir -p $(HTMLDIR)
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix idl       -t "HEALPix/IDL subroutines"   idl
	-$(EQNFIX) $(HTMLDIR)/idl
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_idl
	ln -sf $(CSSFILE) $(HTMLDIR)/idl.css


subroutines.html: $(style) healpix.perl cssfile
	@echo '=================================='
	@echo '      SUBROUTINES.HTML            '
	@echo '=================================='
	mkdir -p $(HTMLDIR)
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix subroutines -t "HEALPix/F90 subroutines" subroutines
	-$(EQNFIX) $(HTMLDIR)/subroutines
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_subroutines
	ln -sf $(CSSFILE) $(HTMLDIR)/subroutines.css

facilities.html: $(style) healpix.perl cssfile
	@echo '=================================='
	@echo '        FACILITIES.HTML           '
	@echo '=================================='
	mkdir -p $(HTMLDIR)
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix facilities -t "HEALPix/F90 facilities"  facilities
	-$(EQNFIX) $(HTMLDIR)/facilities
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_facilities
	ln -sf $(CSSFILE) $(HTMLDIR)/facilities.css

csub.html: $(style) healpix.perl cssfile
	@echo '=================================='
	@echo '            CSUB.HTML             '
	@echo '=================================='
	mkdir -p $(HTMLDIR)
	-$(LATEX2HTML) $(L2HFLAGS) -prefix csub       -t "HEALPix/C subroutines"  csub
	-$(EQNFIX) $(HTMLDIR)/csub
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_csub
	ln -sf $(CSSFILE) $(HTMLDIR)/csub.css

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


