#!/bin/bash

set -eu

shards install
shards build
bin/make config
bin/make neph_config
bin/neph ${FRAMEWORK} --mode=CI --seq
crystal spec