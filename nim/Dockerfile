FROM nimlang/nim:2.2 AS build

WORKDIR /usr/src/app

{{#build_deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -qq update && \
    apt-get -y install {{#build_deps}}{{{.}}} {{/build_deps}} && \
    rm -rf /var/lib/apt/lists/*
{{/build_deps.length}}

ENV NIM_ENV=production \
  NIMBLE_DIR=/home/nim/.nimble \
  PATH=$PATH:/home/nim/.nimble/bin:/root/.nimble/bin

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#build_with}}
  RUN {{{.}}}
{{/build_with}}
{{^build_with}}
  RUN nimble install -y
{{/build_with}}

{{#build_command}}
  RUN {{{.}}}
{{/build_command}}
{{^build_command}}
  RUN nim c {{#build_opts}} {{{.}}} {{/build_opts}} \
    --assertions:off \
    --warnings:off \
    --hints:off \
    -d:release \
    --opt:speed \
    --passC:-flto \
    --passL:-flto \
    server.nim
{{/build_command}}


FROM debian:trixie-slim

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    libssl-dev \
    libpcre2-dev \
    curl \
    {{#runtime_deps}}{{{.}}} {{/runtime_deps}} && \
  rm -rf /var/lib/apt/lists/*

{{#custom_files}}
  COPY --from=build /usr/src/app/{{{.}}} /usr/src/app/{{{.}}}
{{/custom_files}}
{{^custom_files}}
  COPY --from=build /usr/src/app/server /usr/src/app/server
{{/custom_files}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
