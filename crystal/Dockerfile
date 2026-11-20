FROM crystallang/crystal:1.22 AS build

{{#build_deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -qq update && \
    apt-get -y install {{#build_deps}}{{{.}}} {{/build_deps}} && \
    rm -rf /var/lib/apt/lists/*
{{/build_deps.length}}

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN shards lock

RUN --mount=type=cache,id=shards-lib-1.22,target=lib \
  --mount=type=cache,id=shards-cache-1.22,target=/root/.cache/shards \
  shards install --production

RUN --mount=type=cache,id=shards-lib-1.22,target=lib \
  shards build --release --no-debug



FROM debian:trixie-slim

WORKDIR /app

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    {{#runtime_deps}}{{{.}}} {{/runtime_deps}} && \
  rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/src/app/bin/server /app/bin/server

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
