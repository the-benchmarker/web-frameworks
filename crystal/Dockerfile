FROM crystallang/crystal:1.0.0-alpine

WORKDIR /usr/src/app

RUN apk add --update yaml-static

COPY src src/
COPY shard.yml ./

RUN shards install
RUN shards build --release --no-debug --static {{#build_opts}} {{{.}}} {{/build_opts}}

FROM alpine

COPY --from=0 /usr/src/app/bin/server /usr/bin/app

{{#files}}
  COPY --from=0 /usr/src/app/{{.}} /usr/bin/{{{.}}}
{{/files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

WORKDIR /usr/bin
CMD ./app
