# TODO: It would be nice to enable requests like this:
# $ git-local 'list | FILTER "clean {}" | FOREACH "pull {} && extended-status {}"'
# That is, git-local would inject short-named commands into a subshell and
# execute such nice-looking and flexible oneliners.


git-local--clean-p () {
  if [ $# -ne 1 ]; then
    echo "Internal error (invalid ${FUNCNAME[0]} invocation)" >&2
    echo "${FUNCNAME[0]} '$1' '$2' '$3' '$4' '$5'" >&2
    return 255
  fi

  local repostatus=$(cd "$1" && git status)
  test $? -eq 0 || return 1

  if ! echo "$repostatus" | grep -q "working tree clean"; then
    return 1
  fi

  # Different git versions use slightly different spelling: 'up to date' or
  # 'up-to-date'. We handle both.
  if ! echo "$repostatus" | grep -q "Your branch is up.to.date"; then
    return 1
  fi

  return 0
}
export -f git-local--clean-p

git-local--wrap-cmd () {
  if [ $# -ne 1 ]; then
    echo "Internal error (invalid ${FUNCNAME[0]} invocation)" >&2
    echo "${FUNCNAME[0]} '$1' '$2' '$3' '$4' '$5'" >&2
    return 255
  fi
  echo "----------------------------------------"
  eval "$1"
  local rc=$?
  echo
  return $rc
}
export -f git-local--wrap-cmd

git-local-list () {
  # Let command line option override $GIT_LOCAL_SPACE
  if [ $# -eq 0 ]; then
    ECHO "$GIT_LOCAL_SPACE" | tr : $'\n'
  else
    ECHO "$@"
  fi \
  | FOREACH "(test -d {} || (echo 'No such directory: {}' && false)) && ECHO {}" \
  | FOREACH "find -L {} -mindepth 2 -maxdepth 2 -type d -name .git" \
  | FOREACH "ECHO {//}"
  pipestatus
}

git-local-pull () {
  git-local-list "$@" \
  | FOREACH 'git-local--wrap-cmd "echodo cd {} && echodo git pull --rebase" 2>&1'
  pipestatus
}

git-local-status () {
  git-local-list "$@" \
  | FOREACH 'git-local--wrap-cmd "echodo cd {} && echodo git status" 2>&1'
  pipestatus
}

git-local-dirty () {
  git-local-list "$@" \
  | FILTER '! git-local--clean-p {}' \
  | FOREACH 'git-local--wrap-cmd "echodo cd {} && echodo git status" 2>&1'
  pipestatus
}

git-local-dirty-long () {
  git-local-list "$@" \
  | FILTER '! git-local--clean-p {}' \
  | FOREACH 'git-local--wrap-cmd "echodo cd {} && echodo git status && echodo git diff" 2>&1'
  pipestatus
}
