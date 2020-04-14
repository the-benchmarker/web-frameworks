#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
set -vx

# Always runs if master
if [ "${TRAVIS_BRANCH}" == "master" ] ; then
  exit 0
fi

# List of updated files
git diff master.. --name-only  > /tmp/files.txt

FWK=`echo ${FRAMEWORK} | awk -F '.' '{print $1}'`
LNG=`echo ${FRAMEWORK} | awk -F '.' '{print $2}'`

grep "${FWK}" /tmp/files.txt

if [ $? -eq 0 ]; then
  echo "${FWK} has been updated, running CI"
  exit 0
fi

grep "${LNG}" /tmp/files.txt

if [ $? -eq 0 ]; then
  echo "${LNG} has been updated, running CI"
  exit 0
fi

echo "No modification detected, exiting ..."
exit 1
