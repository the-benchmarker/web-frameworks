FROM haskell:9.6-slim

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN stack build --install-ghc

FROM debian:stable-slim

WORKDIR /opt/web

COPY --from=0 /usr/src/app/.stack-work/dist/aarch64-linux/ghc-9.6.6/build/server/server server

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK --interval=900s --start-period=60s --start-interval=60s --timeout=60s --retries=5 CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{#command}}{{{.}}}{{/command}}
