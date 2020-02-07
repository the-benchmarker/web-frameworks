FROM crystallang/crystal:0.32.1-alpine

WORKDIR /usr/src/app

COPY . ./

RUN shards install
RUN shards build --production --no-debug --static {{#build_opts}} {{{.}}} {{/build_opts}}

FROM alpine

COPY --from=0 /usr/src/app/bin/server /usr/bin/app

{{#env_vars}}
{{.}}
{{/env_vars}}

{{#files}}
COPY --from=0 /usr/src/app/{{.}} /usr/bin/{{{.}}}
{{/files}}

WORKDIR /usr/bin
CMD ./app
