FROM crystallang/crystal:1.17-alpine

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN apk update

{{#build_deps}}
  RUN apk add {{{.}}}
{{/build_deps}}

RUN shards lock
RUN shards install --production
RUN shards build --release --no-debug

FROM alpine:latest

WORKDIR /usr/bin

COPY --from=0 /usr/src/app/bin/server /usr/bin/app

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

RUN apk update
RUN apk add curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{command}}
