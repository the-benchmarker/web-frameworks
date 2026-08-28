#!/usr/bin/env bash
set -euo pipefail

DIRECTORY=${DIRECTORY:?DIRECTORY is required}
ENGINE=${ENGINE:?ENGINE is required}
MAX_ATTEMPTS=${MAX_ATTEMPTS:-2}
COMMAND_TIMEOUT_SECONDS=${COMMAND_TIMEOUT_SECONDS:-2700}
RETRY_DELAY_SECONDS=${RETRY_DELAY_SECONDS:-15}

IP_FILE="${DIRECTORY}/ip-${ENGINE}.txt"
CID_FILE="${DIRECTORY}/cid-${ENGINE}.txt"

run_with_timeout() {
  local timeout_seconds=$1
  shift

  set +e
  timeout --kill-after=30s "${timeout_seconds}s" "$@"
  local status=$?
  set -e

  if [ "${status}" -eq 124 ]; then
    echo "Command timed out after ${timeout_seconds}s: $*"
  fi

  return "${status}"
}

wait_for_framework() {
  local ip waited=0 max_wait=120

  ip=$(tr -d '[:space:]' < "${IP_FILE}")
  echo "Waiting for http://${ip}:3000/ to respond (timeout ${max_wait}s)..."

  until curl -s -f "http://${ip}:3000/" > /dev/null; do
    sleep 1
    waited=$((waited + 1))

    if [ "${waited}" -ge "${max_wait}" ]; then
      echo "Container did not become ready in ${max_wait} seconds!"
      echo "---- Container logs ----"
      docker logs "$(tr -d '[:space:]' < "${CID_FILE}")" || true
      return 1
    fi
  done
}

run_once() (
  set -euo pipefail

  cleanup() {
    make -f "${DIRECTORY}/.Makefile" unbuild || true
    make -f "${DIRECTORY}/.Makefile" clean || true
  }

  trap cleanup EXIT

  run_with_timeout "${COMMAND_TIMEOUT_SECONDS}" make -f "${DIRECTORY}/.Makefile" build
  wait_for_framework
  run_with_timeout "${COMMAND_TIMEOUT_SECONDS}" bundle exec rspec .spec
)

attempt=1
status=1

while [ "${attempt}" -le "${MAX_ATTEMPTS}" ]; do
  echo "Framework attempt ${attempt}/${MAX_ATTEMPTS} for ${DIRECTORY} (${ENGINE})"

  if run_once; then
    exit 0
  else
    status=$?
  fi

  if [ "${attempt}" -lt "${MAX_ATTEMPTS}" ]; then
    echo "Retrying in ${RETRY_DELAY_SECONDS}s..."
    sleep "${RETRY_DELAY_SECONDS}"
  fi

  attempt=$((attempt + 1))
done

exit "${status}"
