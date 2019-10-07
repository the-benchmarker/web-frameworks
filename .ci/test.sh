#!/bin/bash

set -eu

shards install
shards build
bin/make config 
bin/neph ${FRAMEWORK} --mode=CI --seq
crystal spec
