FROM debian:13-slim

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive

{{#build_deps.length}}
RUN apt-get -qq update && \
  {{#build_deps}}
    apt-get -qy install --no-install-recommends {{{.}}} && \
  {{/build_deps}}
    apt-get clean && rm -rf /var/lib/apt/lists/*
{{/build_deps.length}}

{{#files}}
  COPY {{source}} {{target}}
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
