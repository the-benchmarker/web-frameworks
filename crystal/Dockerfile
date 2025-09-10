FROM crystallang/crystal:1.17

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -qq update

  {{#deps}}
    RUN apt-get -qy install {{{.}}}
  {{/deps}}

{{/deps.length}}

RUN shards lock
RUN shards install --production
RUN shards build --release --no-debug

FROM ubuntu:24.04

WORKDIR /usr/bin

COPY --from=0 /usr/src/app/bin/server /usr/bin/app

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update
RUN apt-get -y install curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{command}}
