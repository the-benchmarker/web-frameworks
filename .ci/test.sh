#!/bin/bash

set -eu

# create cache directory
mkdir -p /home/travis/docker

shards install
shards build

bin/make config --without-sieger --docker-options="--cache-from=/home/travis/docker/${FRAMEWORK}"
bin/neph ${FRAMEWORK}

# save cache
docker save ${FRAMEWORK} | gzip -c > /home/travis/docker/${FRAMEWORK}.tgz

crystal spec
