FROM ocaml/opam:debian-12-ocaml-5.2

WORKDIR /usr/src/app

USER root
RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends pkg-config libev-dev libssl-dev libgmp-dev curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*
USER opam

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}



{{#command}}
  ENTRYPOINT {{{.}}}
{{/command}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1
