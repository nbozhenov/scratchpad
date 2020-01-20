#!/bin/bash

source ./harness.sh

TEMP_DIR="`mktemp -d`"
EXP="$TEMP_DIR/exp"
REF="$TEMP_DIR/ref"

oneTimeSetUp() {
  true
}

oneTimeTearDown() {
  rm -r "$TEMP_DIR"
}

test_pipestatus_00() {
  (exit 0) | (exit 0)
  pipestatus a 2> "$EXP"
  assertEquals 255 $?
  diff "$EXP" <(echo "pipestatus doesn't take any arguments")
  assertEquals 0 $?
}

test_pipestatus_01() {
  (exit 0)
  pipestatus 2> "$EXP"
  assertEquals 255 $?
  diff "$EXP" <(echo "pipestatus must be invoked immediately after a pipe")
  assertEquals 0 $?
}

test_pipestatus_02() {
  (exit 0) | (exit 0)
  pipestatus
  assertEquals 0 $?
}

test_pipestatus_03() {
  (exit 0) | (exit 0) | (exit 0)
  pipestatus
  assertEquals 0 $?
}

test_pipestatus_04() {
  (exit 4)
  pipestatus 2> "$EXP"
  assertEquals 255 $?
  diff "$EXP" <(echo "pipestatus must be invoked immediately after a pipe")
  assertEquals 0 $?
}

test_pipestatus_05() {
  (exit 0) | (exit 3)
  pipestatus
  assertEquals 3 $?
}

test_pipestatus_06() {
  (exit 5) | (exit 0) | (exit 0)
  pipestatus
  assertEquals 5 $?
}

test_pipestatus_07() {
  (exit 3) | (exit 9) | (exit 24)
  pipestatus
  assertEquals 27 $?
}

source "`which shunit2`"
