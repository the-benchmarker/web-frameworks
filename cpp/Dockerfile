# hadolint ignore=DL3006
FROM alpine

RUN apk add git build-base cmake --no-cache

WORKDIR /usr/src/app

{{#build_deps}}
  RUN apk add {{{.}}} --no-cache
{{/build_deps}}

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

{{#download}}
  RUN {{{.}}}
{{/download}}

{{#build}}
   RUN {{{.}}}
{{/build}}

# hadolint ignore=DL3006
FROM alpine

WORKDIR /opt/web

{{#bin_deps}}
   RUN apk add {{{.}}} --no-cache
{{/bin_deps}}

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

CMD {{#command}}{{{.}}}{{/command}}
