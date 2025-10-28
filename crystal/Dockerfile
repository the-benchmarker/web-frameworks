FROM crystallang/crystal:1.18

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update

{{#build_deps.length}}

  {{#build_deps}}
    RUN apt-get -qy install {{{.}}}
  {{/build_deps}}

{{/build_deps.length}}

RUN apt-get -qy install curl

RUN shards lock
RUN shards install --production
RUN shards build --release --no-debug

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{command}}
