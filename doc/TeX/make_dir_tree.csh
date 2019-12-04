#! /bin/csh

printf "Enter Healpix directory: "
set directory = $<
echo ${directory}
set finalfile = fig/new_dir_tree
set topdir = "Healpix_\\hpxversion/"
#set topdir = "Healpix_3.60/"

# set file   = /tmp/tmp_mytree
# set file2  = /tmp/tmp_mytree2
set file   = /tmp/dir_tree
set file2  = /tmp/dir_tree_tmp
set wrkdir = `dirname $file`
set here = $cwd

cat >! ${file}.tex <<EOF
%======================
\documentclass{article}
\usepackage{dirtree}
\textheight = 14cm
\textwidth = 14cm
\oddsidemargin = 0cm
\topmargin = -2cm
\input{hpxversion}
%----------------------
\begin{document}
\thispagestyle{empty}
\small{
\fbox{
\parbox{0.97\textwidth}{
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
cp -p ${file}.tex ${finalfile}.tex
pdfcrop ${file}.pdf ${finalfile}.pdf
#pdf2png_med ${finalfile}.pdf ${finalfile}.png
#pdf2png_hi ${finalfile}.pdf ${finalfile}.png
#gm convert -density 180 ${finalfile}.pdf[0] ${finalfile}.png
gm convert -density 180 ${finalfile}.pdf ${finalfile}.png
ls -lrt ${finalfile}.*
#mv -f ${file}_crop.pdf ${file}.pdf
#open -a Skim ${finalfile}.pdf
open ${finalfile}.pdf
# display ${finalfile}.png
gm display ${finalfile}.png

exit
