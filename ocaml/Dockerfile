FROM ocaml/opam:debian-12-ocaml-5.2

WORKDIR /usr/src/app

USER root
RUN apt-get update -y && apt-get install pkg-config libev-dev libssl-dev libgmp-dev -y
USER opam

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}


RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK --interval=1m --timeout=60s --retries=3 CMD curl --fail http://0.0.0.0:3000 || exit 1

{{#command}}
  ENTRYPOINT {{{.}}}
{{/command}}
