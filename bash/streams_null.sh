# Это попытка реализовать stream-функции на базе null-terminated строк. Такая
# реализация была бы намного надёжнее в плане корректности, т.к. она могла бы
# корректно работать с любыми данными, если они не содержат нулевых байтов (а
# null не может, например, быть частью имени файлов). В этом файле реализованы
# основные функции и к ним прилагается тест.
#
# Тем не менее от идеи использовать null-terminated строки я в итоге всё же
# отказался. Хотя большинство linux utils и поддерживает работу с
# null-terminated строками, работа с ними не очень удобна (печать в консоль
# промежуточного результата теряет разрывы строк, требуется конвертация
# туда-обратно).
#
# Другая проблема: команда $(echo -n $'\0') не выводит ничего в stdout.
# Возможно, это баг баша, а возможно это by design (bash написан на C, где
# строки оканчиваются нулём, который "не считается"). В любом случае, из-за
# этого периодически происходит потеря null-символов в output.
#
# Возможно, в будущем я добавлю в команды опцию для переключения между двумя
# режимами.
#

# Usage: ECHO [STRING]...
# Form a list of STRING's.
ECHO() {
  for word in "$@"; do
    echo -nE "$word"
    echo -ne '\0'
  done
}
export -f ECHO

# Usage: SPLIT {OPTION|DELIMITER}
# Read stdin and split it into a list breaking the input on DELIMITER
# characters. Supported OPTION's:
#     -n :  split on a newline character
#     -w :  split on any whitespace character (" \t\n")
SPLIT() {
  if [ $# -ne 1 ] || [ ${#1} -ne 1 ]; then
    echo "SPLIT: incorrect invocation" >&2
    usage >&2
    return 255
  fi

  local item=
  while IFS= read -r -d "$1" item; do
    ECHO "$item"
  done
  ECHO "$item"
}
export -f SPLIT

# Usage: FOREACH ACTION
# Read a list from stdin and apply ACTION to each item in the list.
# Exits with non-zero code if any ACTION returned non-zero.
FOREACH() {
  # TODO: Support invocations like this:
  #     ECHO `seq 1 4` | FOREACH -2 "echo {2} x {1}"
  #     > 2 x 1
  #     > 4 x 3
  if [ $# -ne 1 ]; then
    echo "FOREACH: incorrect invocation" >&2
    return 255
  fi
  parallel -n1 --null -j1 --tty "$1"
}
export -f FOREACH

# Usage: FILTER PREDICATE
# Filter a list from stdin invoking PREDICATE for each item.
# When invoked with correct parameters, always exits with 0 exit code.
FILTER() {
  if [ $# -ne 1 ]; then
    echo "FILTER: incorrect invocation" >&2
    return 255
  fi

  FOREACH "( $1 ) && ECHO {} || true"
}
export -f FILTER
