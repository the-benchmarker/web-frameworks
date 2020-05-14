FROM nimlang/nim:1.4.2-alpine

WORKDIR /usr/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

{{#deps}}
  RUN apk --no-cache add {{{.}}}
{{/deps}}

{{#alpine-deps}}
  RUN apk --no-cache add {{{.}}}
{{/alpine-deps}}

ENV PATH $PATH:/root/.nimble/bin

RUN nimble install -y

RUN nim c {{#build_opts}} {{{.}}} {{/build_opts}} \
    --excessiveStackTrace:off \
    -d:release \
    --threads:on \
    server.nim

FROM alpine

WORKDIR /opt/web

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

CMD {{#command}}{{{.}}}{{/command}}
