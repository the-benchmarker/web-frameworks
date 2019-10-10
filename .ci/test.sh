#!/bin/bash

set -eu

shards install
shards build

bin/make config --without-sieger
bin/neph ${FRAMEWORK}

crystal spec
