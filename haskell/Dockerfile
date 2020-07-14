FROM fpco/alpine-haskell-stack:gmp-ghc-8.8.3

WORKDIR /usr/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN stack build

FROM alpine

WORKDIR /opt/web

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

RUN apk add gmp

CMD {{command}}
