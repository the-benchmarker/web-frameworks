FROM debian:trixie-slim AS build

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -y install --no-install-recommends \
    build-essential \
    git \
    cmake \
    {{#build_deps}}{{{.}}} {{/build_deps}} && \
  rm -rf /var/lib/apt/lists/*

ENV GIT_SSL_NO_VERIFY=1

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#download}}
  RUN {{{.}}}
{{/download}}

{{#build}}
  RUN {{{.}}}
{{/build}}



FROM debian:trixie-slim

WORKDIR /opt/web

RUN apt-get -qq update && \
  apt-get -y install --no-install-recommends \
    curl \
    {{#bin_deps}}{{.}} {{/bin_deps}} && \
  rm -rf /var/lib/apt/lists/*

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#binaries}}
  COPY --from=build /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
