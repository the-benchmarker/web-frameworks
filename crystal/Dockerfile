FROM crystallang/crystal:0.34.0-alpine AS build

WORKDIR /usr/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN shards install --production
RUN shards build --release --no-debug --static {{#build_opts}} {{{.}}} {{/build_opts}}

# hadolint ignore=DL3006
FROM alpine

WORKDIR /opt/web

{{#binaries}}
  COPY --from=build /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

{{#files}}
  COPY {{.}} {{{.}}}
{{/files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{#command}}{{{.}}}{{/command}}
