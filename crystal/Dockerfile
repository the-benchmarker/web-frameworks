FROM crystallang/crystal:0.36.1-alpine

WORKDIR /usr/src/app

COPY . ./

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
