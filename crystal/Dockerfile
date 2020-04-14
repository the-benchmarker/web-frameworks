FROM crystallang/crystal:0.34.0-alpine

WORKDIR /usr/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN shards install --production
RUN shards build --release --no-debug --static {{#build_opts}} {{{.}}} {{/build_opts}}

FROM alpine

WORKDIR /opt/web

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

{{#files}}
  COPY {{.}} {{{.}}}
{{/files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD ./bin/server
