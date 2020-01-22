#
# Implementation of STREAM-processing commands
# --------------------------------------------
#
# Some future ideas:
#
# 1) SPLIT() {
#      local delim="$1"
#      shift
#      for word in "$@"; do
#        echo -n "$word" | tr "$delim" $'\n'
#        echo
#      done
#    }
#    Команда обратна команде JOIN. Принимает вход через командную строку, а не
#    stdin, чтобы было очевидно, что она не ждёт string terminator в конце.
#
# 2) SEND { ::: [STRING]... | :::: [FILE]... }...
#    Синтаксис аналогичен gnu parallel, печатает все комбинации в stdout.
#
#
# Примеры использования:
#
# 1) git-local-* tools
#
# 2) Remove nonexistent PATH components:
#    PATH="$(ECHO "$PATH" | tr : $'\n' | FILTER "test -d {}" | JOIN : )"
#


# Usage: ECHO [STRING]...
# Form a list of STRING's.
ECHO() {
  for word in "$@"; do
    echo "$word"
  done
}
export -f ECHO

# Usage: FOREACH ACTION
# Read a list from stdin and apply ACTION to each item in the list.
# Exits with non-zero code if any ACTION returned non-zero.
FOREACH() {
  # TODO: Support invocations like this:
  #     ECHO `seq 1 4` | FOREACH -2 "echo {2} x {1}"
  #     > 2 x 1
  #     > 4 x 3
  if [ $# -ne 1 ]; then
    echo "${FUNCNAME[0]}: incorrect invocation" >&2
    echo "${FUNCNAME[0]} '$1' '$2' '$3' '$4' '$5'" >&2
    return 255
  fi
  parallel -n1 --tty "$1"
}
export -f FOREACH

# Usage: FILTER PREDICATE
# Filter a list from stdin invoking PREDICATE for each item.
# When invoked with correct parameters, always exits with 0 exit code.
FILTER() {
  if [ $# -ne 1 ]; then
    echo "${FUNCNAME[0]}: incorrect invocation" >&2
    echo "${FUNCNAME[0]} '$1' '$2' '$3' '$4' '$5'" >&2
    return 255
  fi

  FOREACH "( $1 ) && ECHO {} || true"
}
export -f FILTER

# Usage: JOIN {OPTION|DELIMITER}
# Read a list from stdin and join lines inserting DELIMITER between lines.
JOIN() {
  # TODO: Support options.
  if [ $# -ne 1 ]; then
    echo "${FUNCNAME[0]}: incorrect invocation" >&2
    echo "${FUNCNAME[0]} '$1' '$2' '$3' '$4' '$5'" >&2
    return 255
  fi

  local delim="$1"
  if [ ${#delim} -ne 1 ]; then
    echo "${FUNCNAME[0]}: incorrect invocation" >&2
    echo "${FUNCNAME[0]} '$1' '$2' '$3' '$4' '$5'" >&2
    return 255
  fi

  local item=
  if IFS= read -r item ; then
    echo -n "$item"
  else
    echo -n "$item"
    return
  fi

  while IFS= read -r item; do
    echo -n "${delim}${item}"
  done
  if [ -n "$item" ]; then
    echo -n "${delim}${item}"
  fi
}
export -f JOIN
