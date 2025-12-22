FROM crystallang/crystal:1.18

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ARG DEBIAN_FRONTEND=noninteractive

{{#build_deps.length}}
RUN apt-get -qq update && \
  {{#build_deps}}
    apt-get -qy install --no-install-recommends {{{.}}} && \
  {{/build_deps}}
    apt-get -qy install --no-install-recommends curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*
{{/build_deps.length}}

{{^build_deps.length}}
RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*
{{/build_deps.length}}

RUN shards lock && \
    shards install --production && \
    shards build --release --no-debug

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{command}}
