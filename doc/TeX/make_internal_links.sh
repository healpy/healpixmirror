#! /bin/sh


listArguments () {
    # listArguments  file begin end
    # returns arguments found in file between begin and end
    file=$1
    sbeg=$2
    send=$3

# cf http://www.folkstalk.com/2013/03/sed-remove-lines-file-unix-examples.html 
    cat ${file} | sed "/${sbeg}/,/${send}/!d" | \
		  grep -v ${sbeg} | grep -v ${send} | \
		  sed "s|\\\\optional||g" | \
		  sed "s|\[||g"  | sed "s|\]||g" | \
		  sed "s|{||g"   | sed "s|}||g"  | \
		  sed "s| ||g"   | sed "s|,|\n|g" | \
		  sed "s|\\\\facname| |g" | sed "s|\\\\FACNAME| |g" | \
		  sed "s|\\\\thedocid||g" | sed "s|\\\\tt||g" | \
		  sed "s/|/ /g" | sed "s|~||g" | sed "s|\\\\hfill||g" | \
	          sed "s|(||g" | sed "s|)||g" 

}

listArgumentsF90Fac () {
    # listArguments  file begin end
    # returns arguments found in file between begin and end
    file=$1
    sbeg=$2
    send=$3

# cf http://www.folkstalk.com/2013/03/sed-remove-lines-file-unix-examples.html 
    cat ${file} | sed "/${sbeg}/,/${send}/!d" | \
		  grep -v ${sbeg} | grep -v ${send} | \
	          grep "fileparam" | sed "s|\\\\fileparam||g" | awk -F= '{print $1}' | \
		  sed "s|\\\\optional||g" | \
		  sed "s|\[||g"  | sed "s|\]||g" | \
		  sed "s|{||g"   | sed "s|}||g"  | \
		  sed "s| ||g"   | sed "s|,|\n|g" | \
		  sed "s|\\\\facname| |g" | sed "s|\\\\FACNAME| |g" | \
		  sed "s|\\\\thedocid||g" | sed "s|\\\\tt||g" | \
		  sed "s/|/ /g" | sed "s|~||g" | sed "s|\\\\hfill||g" | \
	          sed "s|(||g" | sed "s|)||g" 

}


countLinks () {
    file=$1
    np=`grep mylink $file | wc -l`
    echo $np
}

testF90Function () {
    file=$1
    nf=`grep f90function $file | wc -l`
    echo $nf
}


match () {
# emulates Linux `expr match $1 $2` in MacOS
if [[ $1 =~ $2 ]] ; then
 echo ${#BASH_REMATCH}
else
 echo 0
fi
}

#-------------------------------

files=create_alm.tex
if [ ${#} -gt 0 ]; then
    files=$*
fi


magic='MYLINK{ABCdefGHI123'
for shortfile in $files; do

    file="${shortfile%.*}.tex" # make sure .tex suffix is present
    echo $file
    #outfile=`echo $file | sed "s|.tex|_2.tex|"`
    outfile=$file

    np=`countLinks $file`
    ### [[ "$file" = "write_bintabh.tex" ]] && np=0

    if [ $np -eq 0 ] ; then # unprocessed file

	nidl=`match $file "_idl"`
	nfac=`match $file "_fac"`
	if [ "$nidl" = "0"  -a "$nfac" = "0" ] ; then
	    # for F90 functions/subroutines
	    nf=`testF90Function $file`
	    type="F90sub"
	    desc_b="begin{arguments}"
	    desc_e="end{arguments}"
	    pretarg="sub"

	    if [ $nf -eq 0 ] ; then
	        # for F90 subroutines
		form_b="begin{f90format}"
		form_e="end{f90format}"
	    else
	        # for F90 functions
		form_b="begin{f90function}"
		form_e="end{f90function}"
	    fi
	fi
	if [ "$nidl" = "0"  -a "$nfac" > "0" ] ; then
	    # for F90 facilities
	    type="F90fac"
	    form_b="begin{examples}{2}"
	    form_e="end{examples}"
	    desc_b="begin{qualifiers}"
	    desc_e="end{qualifiers}"
	    pretarg="fac"
	fi
	if [ "$nidl" > "0"  -a "$nfac" = "0" ] ; then
	    # for IDL subroutines
	    type="IDL"
	    form_b="begin{IDLformat}"
	    form_e="end{IDLformat}"
	    desc_b1="begin{qualifiers}"
	    desc_e1="end{qualifiers}"
	    desc_b2="begin{keywords}"
	    desc_e2="end{keywords}"
	    pretarg="idl"
	fi
        # --------------------
        rad=`echo $file | sed "s|.tex||"` 
	rad=`echo $rad | sed 's|_idl$||'` # remove trailing _idl
	rad=`echo $rad | sed 's|_fac$||'` # remove trailing _fac
        wrkfile="/tmp/wrkfile_${rad}.txt"
	if [ "$type" = "F90fac" ]; then
	    mylist=`listArgumentsF90Fac $file ${form_b} ${form_e}`
	else
	    mylist=`listArguments $file ${form_b} ${form_e}`
	fi
        
        echo '---------------------------------------------------'
	echo "       $type "
        echo "$file -> $wrkfile -> $outfile"
        cp $file $wrkfile
        for name in $mylist ; do
            tag=`echo $name | sed "s|\\\\\\_|_|g"`
	    tag=`echo $tag | sed "s|/||" | sed "s|=||"` # remove / and =    (for IDL)
            name2=`echo $tag | sed 's|_|\\\\\\\\\\_|g'` # name with \_ but without /,=
            target="\\\\mytarget{${pretarg}:${rad}:${tag}}"
            #link="\\\\mylink{${pretarg}:${rad}:${tag}}{$name}"
            link="\\\\mylink{${pretarg}:${rad}:${tag}}"
	    slink="\\\\mylink{${pretarg}:${rad}"
	    name1=`echo $name | sed "s|\\\\\\_|_|g"`
	    name1=`echo $name1 | sed 's|_|\\\\\\\\\\_|g'` # name with \_,/,=
            # in format section :  replace name with   link name
            # in description section :  replace name with  name target
            #sed -i "/${form_b}/,/${form_e}/s|"${name2}"|${link}{"${name2}"}%\n|" $wrkfile
            #sed -i "/${desc_b}/,/${desc_e}/s|^"${name2}"|${target} "${name2}"|g"  $wrkfile
            #sed -i "/${desc_b}/,/${desc_e}/s|\\\\optional{"${name2}"}|${target} \\\\optional{"${name2}"}|g"  $wrkfile
	    if [ "$type" = "F90sub" ] ; then
		echo "$name \t\t $link \t\t $target \t\t "$name2
		sed -i "/${form_b}/,/${form_e}/s|"${name2}"|${magic}:${tag}}{"${name2}"}%\n|" $wrkfile
		sed -i "/${desc_b}/,/${desc_e}/s|^"${name2}"|"${name2}"${target}|g"  $wrkfile
		sed -i "/${desc_b}/,/${desc_e}/s|\\\\optional{"${name2}"|\\\\optional{"${name2}"${target}|g"  $wrkfile
	    fi
	    if [ "$type" = "IDL" ] ; then
		echo "$name \t\t $link \t\t $target \t\t "$name1" "$name2
		sed -i "/${form_b}/,/${form_e}/s|"${name1}"|${magic}:${tag}}{"${name1}"}%\n|" $wrkfile
		sed -i "/${desc_b1}/,/${desc_e1}/s|^"${name2}"|"${name2}"${target}|g"  $wrkfile
		sed -i "/${desc_b2}/,/${desc_e2}/s|^"${name2}"|"${name2}"${target}|g"  $wrkfile
		sed -i "/${desc_b1}/,/${desc_e1}/s|\\\\item\["${name2}"|\\\\item\["${name2}"${target}%\n|g"  $wrkfile
		sed -i "/${desc_b2}/,/${desc_e2}/s|\\\\item\["${name2}"|\\\\item\["${name2}"${target}%\n|g"  $wrkfile
		sed -i "/${desc_b1}/,/${desc_e1}/s|^/"${name2}"|/"${name2}"${target}|g"  $wrkfile
		sed -i "/${desc_b2}/,/${desc_e2}/s|^/"${name2}"|/"${name2}"${target}|g"  $wrkfile
		sed -i "/${desc_b1}/,/${desc_e1}/s|\\\\item\[/"${name2}"|\\\\item\[/"${name2}"${target}%\n|g"  $wrkfile
		sed -i "/${desc_b2}/,/${desc_e2}/s|\\\\item\[/"${name2}"|\\\\item\[/"${name2}"${target}%\n|g"  $wrkfile
	    fi
	    if [ "$type" = "F90fac" ] ; then
		echo "$name \t\t $link \t\t $target \t\t "$name2
		sed -i "/${form_b}/,/${form_e}/s|"${name2}"|${magic}:${tag}}{"${name2}"}%\n|" $wrkfile
		sed -i "/${desc_b}/,/${desc_e}/s|\\\\item\[{"${name2}" = }\]|\\\\item\[{"${name2}" = }\]${target}%\n|g"  $wrkfile
	    fi
        done
	sed -i "/${form_b}/,/${form_e}/s|${magic}|${slink}|" $wrkfile
        cp -f $wrkfile $outfile
    
    else
        echo "$file $outfile $type already processed."
	[[ ${outfile} != ${file} ]] && cp ${file} ${outfile}
    fi

done

# ./make_internal_links.sh add_card.tex add_dipole.tex alm2cl.tex create_alm.tex
# ./make_internal_links.sh ang2vec bin_llcl
exit
