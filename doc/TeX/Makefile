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
VERSION =  $(shell grep newcommand hpxversion.tex | sed "s/[a-z{}\\]*//g")
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

docs  := intro.tex facilities.tex subroutines.tex idl.tex csub.tex install.tex # install.* must be last
style := healpix.sty # healrut.sty
dvi   := $(docs:.tex=.dvi)
ps    := $(dvi:.dvi=.ps)
gz    := $(dvi:.dvi=.ps.gz)
pdf   := $(docs:.tex=.pdf) pdf_index.pdf
html  := $(docs:.tex=.html)
incs  := hpxversion.tex idlversion.tex gdlversion.tex hpxwebsite.tex healpix_src_url.tex

# define color output if needed
ifeq ($(COLORS),1)
NO_COLOR=\x1b[0m
GREEN_COLOR=\x1b[32;11m
RED_COLOR=\x1b[31;01m
BLUE_COLOR=\x1b[34;11m
endif

pdf: $(pdf)
	cp $(pdf) ../../healpix-dynamicPages/pdf

html: $(html) main.html

$(pdf): %.pdf : %.tex healpix.sty $(incs)
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)              $@                  $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	-$(PDFTEX) $(PDFLLAGS) $< 
	-$(PDFTEX) $(PDFLLAGS) $< >/dev/null
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

intro.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)          INTRO.HTML              $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	mkdir -p $(HTMLDIR)
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix intro -external_file ./intro    -t "Introduction to HEALPix"  intro
	-$(EQNFIX) $(HTMLDIR)/intro > /dev/null
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_intro
	ln -sf $(CSSFILE) $(HTMLDIR)/intro.css

idl.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)            IDL.HTML              $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	mkdir -p $(HTMLDIR)
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix idl       -t "HEALPix/IDL subroutines"   idl
	-$(EQNFIX) $(HTMLDIR)/idl
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_idl
	ln -sf $(CSSFILE) $(HTMLDIR)/idl.css


subroutines.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)      SUBROUTINES.HTML            $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	mkdir -p $(HTMLDIR)
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix subroutines -t "HEALPix/F90 subroutines" subroutines
	-$(EQNFIX) $(HTMLDIR)/subroutines
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_subroutines
	ln -sf $(CSSFILE) $(HTMLDIR)/subroutines.css

facilities.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)        FACILITIES.HTML           $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	mkdir -p $(HTMLDIR)
	-$(LATEX2HTML) $(L2HFLAGS) -split 4 -prefix facilities -t "HEALPix/F90 facilities"  facilities
	-$(EQNFIX) $(HTMLDIR)/facilities
	mv -f $(HTMLDIR)/WARNINGS $(HTMLDIR)/WARNINGS_facilities
	ln -sf $(CSSFILE) $(HTMLDIR)/facilities.css

csub.html: $(style) healpix.perl cssfile
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
	@echo "$(GREEN_COLOR)            CSUB.HTML             $(NO_COLOR)"
	@echo "$(GREEN_COLOR)==================================$(NO_COLOR)"
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


