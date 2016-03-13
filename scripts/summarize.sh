#!/usr/bin/env bash

find `pwd` -maxdepth 2 -type d -name bin -print0 | xargs --null -n1 > "PATH"
find `pwd` -name \*.info\* -exec dirname {} \; | sort -u | xargs -n1 -d $'\n' > "INFOPATH"
