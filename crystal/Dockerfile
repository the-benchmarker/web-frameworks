{{#provider.default.image-version}}
  FROM crystallang/crystal:{{{.}}}
{{/provider.default.image-version}}
{{^provider.default.image-version}}
  FROM crystallang/crystal:1.10
{{/provider.default.image-version}}

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN shards lock
RUN shards install --production
RUN shards build --release --no-debug --static

FROM debian:stable-slim

WORKDIR /usr/bin

COPY --from=0 /usr/src/app/bin/server /usr/bin/app

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{command}}
