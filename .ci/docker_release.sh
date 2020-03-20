#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'
set -vx

echo ${DOCKER_PASSWORD} | docker login -u ${DOCKER_USERNAME} --password-stdin

docker tag ${FRAMEWORK} ${DOCKER_USERNAME}/${FRAMEWORK}

docker push ${DOCKER_USERNAME}/${FRAMEWORK}
