#!/bin/bash

set -eu

# create cache dir, if no exists
mkdir -p ${HOME}/docker

# save image not in dandling state
docker images -a --filter='dangling=false' --format '{{.Repository}}:{{.Tag}} {{.ID}}' | xargs -n 2 -t sh -c 'test -e $HOME/docker/$1.tar.gz || docker save $0 | gzip -2 > $HOME/docker/$1.tar.gz'