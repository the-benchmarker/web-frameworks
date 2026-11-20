FROM debian:trixie-slim AS build

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -y install --no-install-recommends \
    build-essential \
    wget \
    {{#build_deps}}{{{.}}} {{/build_deps}} && \
  rm -rf /var/lib/apt/lists/*

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
  apt-get -qy install --no-install-recommends \
    curl \
    {{#bin_deps}}{{{.}}} {{/bin_deps}} && \
  rm -rf /var/lib/apt/lists/*

{{#binaries}}
  COPY --from=build /usr/src/app/{{.}} {{{.}}}
{{/binaries}}

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
