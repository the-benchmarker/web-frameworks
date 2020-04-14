FROM crystallang/crystal:0.34.0

WORKDIR /usr/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN shards install --production
RUN shards build --release --no-debug

FROM alpine

WORKDIR /opt/web

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}
