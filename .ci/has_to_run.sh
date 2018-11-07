#!/bin/bash

set +e

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


# List of updated files
git diff ${TRAVIS_COMMIT_RANGE} --name-only > /tmp/changed

# If current framework was updated
grep -q "$FRAMEWORK" /tmp/changed

if [[ $? != 0 ]]; then
  echo "$FRAMEWORK was not modified, exiting."
  exit 1
fi

