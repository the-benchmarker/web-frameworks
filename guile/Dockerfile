FROM debian:trixie-slim

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -y update

{{#build_deps.length}}

  {{#build_deps}}
    RUN apt-get -y install {{{.}}}
  {{/build_deps}}

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

RUN apt-get -qy install curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
