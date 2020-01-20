#!/bin/bash

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
  diff "$EXP" <(echo -ne "a\n")
  assertEquals 0 $?
}

test_ECHO_03() {
  ECHO abc d ef "" > "$EXP"
  assertEquals 0 $?
  assertEquals 10 `cat "$EXP" | wc -c`
  diff "$EXP" <(echo -ne "abc\nd\nef\n\n")
  assertEquals 0 $?
}

test_ECHO_04() {
  ECHO "a'b" "`echo -ne '\03\05\07'`" 'a`' | hexdump > "$EXP"
  pipestatus
  assertEquals 0 $?

  cat <<EOF > "$REF"
0000000 2761 0a62 0503 0a07 6061 000a
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
0000000 0920 610a 6220 0a0a 0978 000a
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
0000000 9b1b 2233 0a50
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
  echo -ne "1\n2\n3\n" | FOREACH "echo {}" > "$EXP"
  assertEquals 0 $?
  diff "$EXP" <(echo -ne "1\n2\n3\n")
  assertEquals 0 $?
}

test_FOREACH_03() {
  echo -ne '/a b/`!.def'$'\n'"/  '/ ~\t\n / ,.)" \
  | FOREACH "echo -nE {}; echo -ne '\0'" \
  | hexdump > "$EXP"
  pipestatus
  assertEquals 0 $?

  cat <<EOF > "$REF"
0000000 612f 6220 602f 2e21 6564 0066 202f 2720
0000010 202f 097e 2000 202f 2e2c 0029
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
0000000 601b 0920 1022 0a27
0000008
EOF
  diff -w "$EXP" "$REF"
  assertEquals 0 $?
}

test_JOIN_01() {
  ECHO | JOIN : > "$EXP"
  assertEquals 0 $?
  assertEquals 0 `cat "$EXP" | wc -c`
}

test_JOIN_02() {
  ECHO a | JOIN : > "$EXP"
  assertEquals 0 $?
  diff "$EXP" <(echo -ne "a")
  assertEquals 0 $?
}

test_JOIN_03() {
  ECHO a b | JOIN : > "$EXP"
  assertEquals 0 $?
  diff "$EXP" <(echo -ne "a:b")
  assertEquals 0 $?
}

test_JOIN_04() {
  ECHO a b c | JOIN : > "$EXP"
  assertEquals 0 $?
  diff "$EXP" <(echo -ne "a:b:c")
  assertEquals 0 $?
}

test_JOIN_05() {
  echo -n a | JOIN : > "$EXP"
  assertEquals 0 $?
  diff "$EXP" <(echo -ne "a")
  assertEquals 0 $?
}

test_JOIN_06() {
  echo -ne "a\nb\nc" | JOIN : > "$EXP"
  assertEquals 0 $?
  diff "$EXP" <(echo -ne "a:b:c")
  assertEquals 0 $?
}

test_JOIN_07() {
  # ESC ` SPACE \t " \n ' ++ an escape sequence
  ECHO $'\x1b'$'\x60'$'\x20'$'\x09'$'\x22'$'\x10'$'\x27' $'\x1b'$'\x9b'$'\x33'$'\x22'$'\x50' \
  | JOIN $'\t' | hexdump > "$EXP"
  pipestatus
  assertEquals 0 $?

  cat <<EOF > "$REF"
0000000 601b 0920 1022 0927 9b1b 2233 0050
000000d
EOF
  diff -w "$EXP" "$REF"
  assertEquals 0 $?
}


source "`which shunit2`"
