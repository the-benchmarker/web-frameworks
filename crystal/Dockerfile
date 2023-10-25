{{#provider.default.image-version}}
  FROM crystallang/crystal:{{{.}}}-alpine
{{/provider.default.image-version}}
{{^provider.default.image-version}}
  FROM crystallang/crystal:1.10.1-alpine
{{/provider.default.image-version}}

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

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
  COPY '{{source}}' '{{target}}'
{{/static_files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{command}}
