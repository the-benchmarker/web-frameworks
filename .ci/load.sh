#!/bin/bash

set -eu

find ${HOME}/docker -type f -name '*.tar.gz' > /tmp/list.txt

while read image ; do
  zcat ${image} | docker load
done < /tmp/list.txt