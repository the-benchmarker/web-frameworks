FROM jrei/crystal-alpine:0.31.1

WORKDIR /usr/src/app

COPY . ./

RUN shards install
RUN shards build --production --release --no-debug --static {{#build_opts}} {{{.}}} {{/build_opts}}

FROM alpine

COPY --from=0 /usr/src/app/bin/server /usr/bin/app

{{#files}}
COPY --from=0 /usr/src/app/{{.}} /usr/bin/{{{.}}}
{{/files}}

WORKDIR /usr/bin
CMD ./app
