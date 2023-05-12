{{#provider.default.image-version}}
  FROM crystallang/crystal:{{{.}}}-alpine
{{/provider.default.image-version}}
{{^provider.default.image-version}}
  FROM crystallang/crystal:1.8.2-alpine
{{/provider.default.image-version}}

WORKDIR /usr/src/app

COPY . ./

RUN apk add --update yaml-static

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
