#abspath() {
#  env abspath "$@"
#}

fail() {
  echo "ERROR:${1}: ${2}" >&2
  exit 1
}

y-join() {
  local delim="$1"
  local first=1
  shift
  for i in "$@"; do
    if [ "$first" -eq 0 ]; then
      echo -n "$delim"
    fi
    echo -n "$i"
    first=0
  done
}

hl() {
  grep --color=always -E "`y-join \| "$@" ''`"
}

swap() {
  local dir1=`dirname $(abspath "$1")`
  local tmpname=`mktemp --tmpdir=$(dirname "$1") -u $(basename "$1").XXXXXX`
  ln -T "$1" "$tmpname"
  mv "$2" "$1"
  mv "$tmpname" "$2"
}

not () {
  if "$@"; then
    return 1
  else
    return 0
  fi
}

#
# example:
# $ echo /etc /etc/fstab /home /nonexistent | y_filter "test -d placeholder"
# /etc /home
#
y_filter () {
    local elt=
    xargs -n1 | while read elt; do
        eval "${1//placeholder/$elt}" && echo $elt
    done
}

y_merge () {
    local len=${#1}
    local len=$(( len + 1 ))
    xargs -n1 -I param echo -n ${1}param | cut -c ${len}-
}

#
# example:
# $ export INFOPATH=/old
# $ y_append INFOPATH /new1 /new2
# $ echo $INFOPATH
# /old:/new1:/new2
#
y_append () {
    if [ $# -lt 1 ]; then
        echo "y_append: invalid usage" >&2
        return 1
    fi

    local appendee=$1
    local append=
    local old_value=$(eval "echo -n \$$appendee")
    local cmd=
    shift

    for append in "$@"; do
        if [ -z "$old_value" ]; then
            cmd="export $appendee=$append"
            old_value=old_value
        else
            cmd="export $appendee=\$$appendee:$append"
        fi
        eval "$cmd"
    done
}

#
# example:
# $ export INFOPATH=/old
# $ y_prepend INFOPATH /new1 /new2 /new3
# $ echo $INFOPATH
# /new3:/new2:/new1:/old
#
y_prepend () {
    if [ $# -lt 1 ]; then
        echo "y_prepend: invalid usage" >&2
        return 1
    fi

    local prependee=$1
    local prepend=
    local old_value=$(eval "echo -n \$$prependee")
    local cmd=
    shift

    for prepend in "$@"; do
        if [ -z "$old_value" ]; then
            cmd="export $prependee=$prepend"
            old_value=old_value
        else
            cmd="export $prependee=$prepend:\$$prependee"
        fi
        eval "$cmd"
    done
}
