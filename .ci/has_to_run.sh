#!/bin/bash

travis_terminate() {
  if [[ ! "${TRAVIS_OS_NAME}" ]]; then
    return
  fi

  "_travis_terminate_${TRAVIS_OS_NAME}" "${@}"
}

_travis_terminate_linux() {
  _travis_terminate_unix "${@}"
}

_travis_terminate_osx() {
  _travis_terminate_unix "${@}"
}

_travis_terminate_unix() {
  set +e
  [[ "${TRAVIS_FILTERED}" == redirect_io && -e /dev/fd/9 ]] &&
    sync &&
    command exec 1>&9 2>&9 9>&- &&
    sync
  pgrep -u "${USER}" | grep -v -w "${$}" >"${TRAVIS_TMPDIR}/pids_after"
  awk 'NR==FNR{a[$1]++;next};!($1 in a)' "${TRAVIS_TMPDIR}"/pids_{before,after} |
    xargs kill &>/dev/null || true
  pkill -9 -P "${$}" &>/dev/null || true
  exit "${1}"
}


CURRENT_BRANCH=${TRAVIS_PULL_REQUEST_BRANCH:-$TRAVIS_BRANCH}

if [[ -z ${TRAVIS_PULL_REQUEST_SLUG} ]] ; then
  echo "This is a branch build"
  exit(0)
if

if [[ ${CURRENT_BRANCH} == "master" && !-z ${TRAVIS_PULL_REQUEST_SLUG} ]] ; then
  echo "This is a master build"
  exit(0)
fi

if [[ ${CURRENT_BRANCH} == dependabot* && !-z ${TRAVIS_PULL_REQUEST_SLUG} ]] ; then
  echo "This is a branch from dependabot"
  exit(1)
fi

if [[ ${CURRENT_BRANCH} == renovate* && !-z ${TRAVIS_PULL_REQUEST_SLUG} ]] ; then
  echo "This is a branch from renovate"
  exit(1)
fi

# List of updated files
git diff master.. --name-only > /tmp/list.txt

echo "Modified files are"
cat /tmp/list.txt

LNG=`echo ${FRAMEWORK} | awk -F '.' '{print $1}'`
FWK=`echo ${FRAMEWORK} | awk -F '.' '{print $2}'`

grep "^${LNG}/" /tmp/list.txt && exit 0
grep "^${LNG}/${FWK}/" /tmp/list.txt && exit 0

echo "No modification, exiting ..."
exit 1
