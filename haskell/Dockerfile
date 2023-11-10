FROM haskell:9.8-slim

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN stack build --install-ghc

FROM debian:stable-slim

WORKDIR /opt/web

COPY --from=0 /usr/src/app/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/server/server server

RUN apk add gmp

CMD {{command}}
