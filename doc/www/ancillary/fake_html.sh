#! /bin/sh


suffix='.html'
path='' # with trailing / if not empty

#=========================================================
write_html () {
    php=$1
    htm=$2
    path=$3
    echo "$htm:"

#\rm -f $htm
cat <<EOF> $htm
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
  <head>
    <title>HEALPix</title>
    <meta http-equiv="REFRESH" content="0;url=${path}${php}"/>
  </head>
  <body>
  </body>
</html>
EOF

    cat $htm
    echo '---------------------------------------------------'
}

Main(){

    list=$(ls *.php | grep -v '\.inc\.php')

    for php in ${list}; do
	htm=`echo ${php} | sed "s/\.php/${suffix}/g"`
	write_html   ${php} ${htm} ${path}
    done

}
#=========================================================

Main

exit

