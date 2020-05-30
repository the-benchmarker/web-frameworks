FROM fpco/alpine-haskell-stack:gmp-ghc-8.8.3

WORKDIR /opt/web

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN stack build
