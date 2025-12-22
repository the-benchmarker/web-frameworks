FROM haskell:9.12-bookworm

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN stack build --install-ghc

FROM debian:trixie-slim

WORKDIR /opt/web

COPY --from=0 /usr/src/app/.stack-work/dist/{{arch}}-linux-tinfo6/ghc-9.6.6/build/server/server server 

RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
