FROM crystallang/crystal:1.7.3-alpine

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

COPY --from=0 /usr/src/app/bin/server /usr/bin/app

{{#files}}
  COPY --from=0 /usr/src/app/{{.}} /usr/bin/{{{.}}}
{{/files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

WORKDIR /usr/bin
CMD ./app
