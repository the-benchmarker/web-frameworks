FROM haskell:9.6-slim

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN stack build --install-ghc

FROM debian:stable-slim

WORKDIR /opt/web

COPY --from=0 /usr/src/app/.stack-work/dist/aarch64-linux-tinfo6-libc6-pre232/ghc-9.6.6/build/server/server server

CMD {{command}}
