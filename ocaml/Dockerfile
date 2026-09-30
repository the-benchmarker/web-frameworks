FROM ocaml/opam:debian-13-ocaml-5.2

WORKDIR /usr/src/app

# Switch to root to install system dependencies
USER root

RUN apt-get -qq update && \
  apt-get -qy install \
    pkg-config \
    libev-dev \
    libssl-dev \
    libgmp-dev \
    curl && \
  rm -rf /var/lib/apt/lists/*

USER opam

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
