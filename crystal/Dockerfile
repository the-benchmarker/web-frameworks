FROM crystallang/crystal:1.7.2-alpine

WORKDIR /usr/src/app

COPY . ./

RUN apk add --update yaml-static

{{#build_deps}}
RUN apk add --update {{{.}}}
{{/build_deps}}

RUN shards lock
RUN shards install --production
RUN shards build --release --no-debug --static {{#build_opts}} {{{.}}} {{/build_opts}}

FROM alpine

WORKDIR /usr/bin

COPY --from=0 /usr/src/app/bin/server /usr/bin/app

{{#static_files}}
  COPY --from=0 /usr/src/app/{{.}} /usr/bin/{{{.}}}
{{/static_files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{command}}
