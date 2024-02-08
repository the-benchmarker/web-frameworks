FROM haskell:9.8-slim

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN stack build --install-ghc

FROM debian:stable-slim

WORKDIR /opt/web

COPY --from=0 /usr/src/app/.stack-work/dist/x86_64-linux-tinfo6-libc6-pre232/ghc-8.8.3/build/server/server server

CMD {{command}}
