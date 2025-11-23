#!/bin/bash

# Due to the fact that we are disabling plugins when installing/updating/downgrading composer dependencies
# we have to manually enable the coding standard here.
composer enable-codestandard
