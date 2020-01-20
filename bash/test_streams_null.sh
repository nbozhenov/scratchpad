#!/bin/bash

# Source streams_null.sh explicitly in order to override non-null versions of
# the commands.
source ./streams_null.sh

TEMP_DIR="`mktemp -d`"
EXP="$TEMP_DIR/exp"
REF="$TEMP_DIR/ref"

oneTimeSetUp() {
  true
}

oneTimeTearDown() {
  rm -r "$TEMP_DIR"
}

test_ECHO_01() {
  ECHO > "$EXP"
  assertEquals 0 $?
  assertEquals 0 `cat "$EXP" | wc -c`
}

test_ECHO_02() {
  ECHO a > "$EXP"
  assertEquals 0 $?
  assertEquals 2 `cat "$EXP" | wc -c`
  diff "$EXP" <(echo -ne "a\0")
  assertEquals 0 $?
}

test_ECHO_03() {
  ECHO abc d ef "" > "$EXP"
  assertEquals 0 $?
  assertEquals 10 `cat "$EXP" | wc -c`
  diff "$EXP" <(echo -ne "abc\0d\0ef\0\0")
  assertEquals 0 $?
}

test_ECHO_04() {
  ECHO "a'b" "`echo -ne '\03\05\07'`" 'a`' | hexdump > "$EXP"
  pipestatus
  assertEquals 0 $?

  cat <<EOF > "$REF"
0000000 2761 0062 0503 0007 6061 0000
000000b
EOF
  diff -w "$EXP" "$REF"
  assertEquals 0 $?
}

test_ECHO_05() {
  ECHO "`echo -ne ' \t'`" "a b" "`echo -ne '\nx\t'`" | hexdump > "$EXP"
  pipestatus
  assertEquals 0 $?

  cat <<EOF > "$REF"
0000000 0920 6100 6220 0a00 0978 0000
000000b
EOF
  diff -w "$EXP" "$REF"
  assertEquals 0 $?
}

test_ECHO_06() {
  # Escape sequence.
  ECHO $'\x1b'$'\x9b'$'\x33'$'\x22'$'\x50' | hexdump > "$EXP"
  pipestatus
  assertEquals 0 $?

  cat <<EOF > "$REF"
0000000 9b1b 2233 0050
0000006
EOF
  diff -w "$EXP" "$REF"
  assertEquals 0 $?
}

test_SPLIT_01() {
  echo -n "" | SPLIT . > "$EXP"
  assertEquals 0 $?
  assertEquals 1 `cat "$EXP" | wc -c`
  diff "$EXP" <(echo -ne "\0")
  assertEquals 0 $?
}

test_SPLIT_02() {
  echo -n "ab" | SPLIT . > "$EXP"
  assertEquals 0 $?
  diff "$EXP" <(echo -ne "ab\0")
  assertEquals 0 $?
}

test_SPLIT_03() {
  echo -n "def::ghi" | SPLIT : > "$EXP"
  assertEquals 0 $?
  diff "$EXP" <(echo -ne "def\0\0ghi\0")
  assertEquals 0 $?
}

test_SPLIT_04() {
  echo -n "?def::ghi?" | SPLIT '?' > "$EXP"
  assertEquals 0 $?
  diff "$EXP" <(echo -ne "\0def::ghi\0\0")
  assertEquals 0 $?
}

test_SPLIT_05() {
  # input: \ta\n`'!\n\"
  echo -n $'\x09'$'\x61'$'\n'$'\x60'$'\x27'$'\x21'$'\n'$'\x22' | SPLIT $'\n' | hexdump > "$EXP"
  pipestatus
  assertEquals 0 $?

  cat <<EOF > "$REF"
0000000 6109 6000 2127 2200 0000
0000009
EOF
  diff -w "$EXP" "$REF"
  assertEquals 0 $?
}

test_SPLIT_06() {
  echo -n $'\x05'$'\x03'$'\x02'$'\x08'$'\x07'$'\x03'$'\x02'$'\x03'$'\x04' \
  | SPLIT $'\x03' | hexdump > "$EXP"
  pipestatus
  assertEquals 0 $?

  cat <<EOF > "$REF"
0000000 0005 0802 0007 0002 0004
000000a
EOF
  diff -w "$EXP" "$REF"
  assertEquals 0 $?
}

test_SPLIT_07() {
  ECHO a b c | SPLIT b | hexdump > "$EXP"
  pipestatus
  assertEquals 0 $?

  # TODO: in the current implementation SPLIT drops \0 bytes from the input
  # stream.
  cat <<EOF > "$REF"
0000000 0061 0000 0063
0000006
EOF
  diff -w "$EXP" "$REF"
  assertEquals 0 $?
}

test_FOREACH_01() {
  echo -n "" | FOREACH "echo {}" > "$EXP"
  assertEquals 0 $?
  assertEquals 0 `cat "$EXP" | wc -c`
}

test_FOREACH_02() {
  echo -ne "1\x002\x003\x00" | FOREACH "echo {}" > "$EXP"
  assertEquals 0 $?
  diff "$EXP" <(echo -ne "1\n2\n3\n")
  assertEquals 0 $?
}

test_FOREACH_03() {
  echo -ne '/a b/`!.def':"/  '/ ~\t\n / ,.)" \
  | SPLIT : \
  | FOREACH "echo -nE {}; echo -ne '\0'" \
  | hexdump > "$EXP"
  pipestatus
  assertEquals 0 $?

  cat <<EOF > "$REF"
0000000 612f 6220 602f 2e21 6564 0066 202f 2720
0000010 202f 097e 200a 202f 2e2c 0029
000001c
EOF
  diff -w "$EXP" "$REF"
  assertEquals 0 $?
}

test_FOREACH_04() {
  ECHO 0 0 0 | FOREACH "exit {}"
  assertEquals 0 $?
  ECHO 0 1 0 | FOREACH "exit {}"
  assertEquals 1 $?
}

test_FOREACH_05() {
  ECHO 0 1 0 | FOREACH "echo -n {} && exit {}" > "$EXP"
  assertEquals 1 $?
  diff "$EXP" <(echo -ne "010")
  assertEquals 0 $?
}

test_FILTER_01() {
  echo -n | FILTER 'true' > "$EXP"
  assertEquals 0 $?
  assertEquals 0 `cat "$EXP" | wc -c`
}

test_FILTER_02() {
  ECHO 0 1 2 3 4 5 | FILTER '[ $(( {} & 1 )) -eq 1 ]' > "$EXP"
  assertEquals 0 $?
  diff "$EXP" <(ECHO 1 3 5)
  assertEquals 0 $?
}

test_FILTER_03() {
  # ESC ` SPACE \t " \n '
  ECHO $'\x1b'$'\x60'$'\x20'$'\x09'$'\x22'$'\x10'$'\x27' | FILTER true | hexdump > "$EXP"
  pipestatus
  assertEquals 0 $?
  cat <<EOF > "$REF"
0000000 601b 0920 1022 0027
0000008
EOF
  diff -w "$EXP" "$REF"
  assertEquals 0 $?
}

source "`which shunit2`"
