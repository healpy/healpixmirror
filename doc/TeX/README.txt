directory for LaTeX files generating HTML and PDF documentation


scripts/Makefiles:
	Makefile
	eqnfix.sh
	update_module_path.sh

indexes:
	pdf_index.tex
	main.html

top LaTeX files:
	idl.tex 
		*_idl.tex
	install.tex
		none
	intro.tex 	
		none
	facilities.tex 

	subroutines.tex 

	csub.tex 
		*_c.tex

input files:
	hpxversion.tex
	idlversion.tex
	gdlversion.tex
	hpxwebsite.tex
	healpix_src_url.tex   (used by healpix.perl)


style files:
	healpix.sty
	healpix.perl
	common.css
	(healrut.sty)



see also fig/process_images.txt

