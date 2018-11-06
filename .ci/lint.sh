#!/usr/bin/env sh

sudo pip install yamllint

find . -type f -name '*.yml' -or -name '*.yaml' -exec yamllint {} \;
