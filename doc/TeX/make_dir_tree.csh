#! /bin/csh

set directory = /tmp/Healpix_3.60 # to be changed
set finalfile = fig/new_dir_tree
set topdir = "Healpix_\hpxversion/"
#set topdir = "Healpix_3.60/"

set file   = /tmp/tmp_mytree
set file2  = /tmp/tmp_mytree2
set wrkdir = `dirname $file`
set here = $cwd

cat >! ${file}.tex <<EOF
%======================
\documentclass{article}
\usepackage{dirtree}
\textheight = 15cm
\textwidth = 15cm
\oddsidemargin = 0cm
\topmargin = -2cm
\input{hpxversion}
%----------------------
\begin{document}
\thispagestyle{empty}
\small{
\fbox{
\parbox{0.95\textwidth}{
\dirtree{%
EOF
cd ${directory} ; tree -d -L 3 -n -F  | \
    sed 's|\|   \|   .-- \(.*\)| .3 \1/. |g' | \
    sed 's|\|   .-- \(.*\)|  .2 \1/. NL .3 |g' | \
    sed 's|.-- \(.*\)| .1 \1/. |g' | \
    sed 's|^\.| .1 '${topdir}'.|g' | \
    sed 's|_|\\_|g' | \
    grep . | \
    grep -v " directories" >! ${file2}.tex
cd ${here}

cat ${file2}.tex | \
    sed -e :a -e '$\!N;s/\n .3 \(.*\)./\1, /;ta' -e 'P;D' | \
    sed 's/\., /, /g' | \
    sed 's/, $/./g'   | \
    sed 's/NL .3 $//g' | \
    sed 's/NL .3 /\\
   .3 /g' | \
    grep -v 'ximview' >> ${file}.tex

cat >> ${file}.tex <<EOF
}
}
}
}
\end{document}
%----------------------
%======================
EOF

cat ${file}.tex
cp -f hpxversion.tex ${wrkdir}
pdflatex -output-directory ${wrkdir} ${file}
pdflatex -output-directory ${wrkdir} ${file}
pdfcrop ${file}.pdf ${finalfile}.pdf
pdf2png_med ${finalefile}.pdf ${finalfile}.png
#mv -f ${file}_crop.pdf ${file}.pdf
open -a Skim ${finalfile}.pdf
display ${finalfile}.png

exit
