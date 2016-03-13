#!/usr/bin/env sh

scriptdir=`dirname $0`
emacs -l ~/.emacs --batch --eval "(byte-recompile-directory \"$scriptdir\" 0 t)"
