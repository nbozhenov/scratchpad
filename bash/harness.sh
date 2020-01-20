echodo() {
  echo + "$@"
  "$@"
}
export -f echodo

pipestatus() {
  local status=(${PIPESTATUS[@]})

  if [ $# -ne 0 ]; then
    echo "pipestatus doesn't take any arguments" >&2
    return 255
  fi

  if [ ${#status[@]} -le 1 ]; then
    echo "pipestatus must be invoked immediately after a pipe" >&2
    return 255
  fi

  local total=0
  for i in ${status[@]}; do
    total=$(( total | i ))
  done

  return $total
}
export -f pipestatus

swap() {
  local dir1=`dirname $(abspath "$1")`
  local tmpname=`mktemp --tmpdir=$(dirname "$1") -u $(basename "$1").XXXXXX`
  ln -T "$1" "$tmpname"
  mv "$2" "$1"
  mv "$tmpname" "$2"
}

#
# example:
# $ export INFOPATH=/old
# $ x_append INFOPATH /new1 /new2
# $ echo $INFOPATH
# /old:/new1:/new2
#
x_append () {
    if [ $# -lt 1 ]; then
        echo "x_append: invalid usage" >&2
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
# $ x_prepend INFOPATH /new1 /new2 /new3
# $ echo $INFOPATH
# /new3:/new2:/new1:/old
#
x_prepend () {
    if [ $# -lt 1 ]; then
        echo "x_prepend: invalid usage" >&2
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
