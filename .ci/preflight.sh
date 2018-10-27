#!/bin/bash

set -e

# List updated frameworks
git diff --name-only master...${TRAVIS_COMMIT} | awk -F '/' '{print $2}' | sort | uniq | sed '/^$/d' > /tmp/changed

# If current framework was updated
grep -q "^$FRAMEWORK$" /tmp/changed

if [[ $? != 0 ]]; then
  echo "$FRAMEWORK was not modified, exiting."
  travis_terminate 0
  exit 1	
fi
