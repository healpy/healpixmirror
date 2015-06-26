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
		  sed "s| ||g"   | sed "s|,|\n|g"

}


testFile () {
    file=$1
    np=`grep mylink $file | wc -l`
    echo $np
}


match () {
# emulates Linux `expr match $1 $2` in MacOS
if [ $1 =~ $2 ] ; then
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

type="F90"

magic='MYLINK{ABCdefGHI123'
for file in $files; do
    np=`testFile $file`
    if [ $np -eq 0 ] ; then

	nidl=`match $file "_idl"`
	if [ "$nidl" = "0" ] ; then
	    # for F90 subroutines
	    type="F90"
	    form_b="begin{f90format}"
	    form_e="end{f90format}"
	    desc_b="begin{arguments}"
	    desc_e="end{arguments}"
	    pretarg="sub"
	else
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
        mylist=`listArguments $file ${form_b} ${form_e}`
        rad=`echo $file | sed "s|.tex||"` 
        wrkfile="/tmp/wrkfile_${rad}.txt"
        outfile=`echo $file | sed "s|.tex|_2.tex|"`
        
        #echo $mylist
        echo '---------------------------------------------------'
	echo "       $type "
        echo "$file -> $wrkfile -> $outfile"
        cp $file $wrkfile
        for name in $mylist ; do
            tag=`echo $name | sed "s|\\\\\\_|_|g"`
	    tag=`echo $tag | sed "s|/||" | sed "s|=||"` # remove / and =    (for IDL)
            name2=`echo $tag | sed 's|_|\\\\\\\\\\_|g'`
            target="\\\\mytarget{${pretarg}:${rad}:${tag}}"
            #link="\\\\mylink{${pretarg}:${rad}:${tag}}{$name}"
            link="\\\\mylink{${pretarg}:${rad}:${tag}}"
	    slink="\\\\mylink{${pretarg}:${rad}"
            echo "$name \t\t $link \t\t $target $name"
            # in format section :  replace name with   link name
            #sed -i "/${form_b}/,/${form_e}/s|"${name2}"|${link}{"${name2}"}%\n|" $wrkfile
            sed -i "/${form_b}/,/${form_e}/s|"${name2}"|${magic}:${tag}}{"${name2}"}%\n|" $wrkfile
            # description section :  replace name with  target name
            #sed -i "/${desc_b}/,/${desc_e}/s|^"${name2}"|${target} "${name2}"|g"  $wrkfile
            #sed -i "/${desc_b}/,/${desc_e}/s|\\\\optional{"${name2}"}|${target} \\\\optional{"${name2}"}|g"  $wrkfile
            # description section :  replace name with  name target
	    if [ "$type" = "F90" ] ; then
		sed -i "/${desc_b}/,/${desc_e}/s|^"${name2}"|"${name2}"${target}|g"  $wrkfile
		sed -i "/${desc_b}/,/${desc_e}/s|\\\\optional{"${name2}"|\\\\optional{"${name2}"${target}|g"  $wrkfile
	    fi
	    if [ "$type" = "IDL" ] ; then
		sed -i "/${desc_b1}/,/${desc_e1}/s|^"${name2}"|"${name2}"${target}|g"  $wrkfile
		sed -i "/${desc_b2}/,/${desc_e2}/s|^"${name2}"|"${name2}"${target}|g"  $wrkfile
		sed -i "/${desc_b1}/,/${desc_e1}/s|\\\\item\["${name2}"\]|\\\\item\["${name2}"\] ${target}%\n|g"  $wrkfile
		sed -i "/${desc_b2}/,/${desc_e2}/s|\\\\item\["${name2}"\]|\\\\item\["${name2}"\] ${target}%\n|g"  $wrkfile
	    fi
        done
	sed -i "/${form_b}/,/${form_e}/s|${magic}|${slink}|" $wrkfile
        cp -f $wrkfile $outfile
    
    else
        echo "$file already processed."
    fi

done

# ./make_internal_links.sh add_card.tex add_dipole.tex alm2cl.tex create_alm.tex
exit
