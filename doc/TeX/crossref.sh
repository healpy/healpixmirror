#!/bin/sh

# resolve cross-references between 2 different html documents produced by latex2html


if [ -z "$1" ]; then
  cat <<EOF
Resolve cross-references
Usage: $0 calling_html target_html_1 [target_html_2 target_html_3 ...]
EOF
  exit
fi


calling=$1
t1=$2

#echo ${calling} ${t1}

index=${t1}"index.pl"

echo ${calling} ${index}

