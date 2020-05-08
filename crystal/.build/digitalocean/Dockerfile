FROM crystallang/crystal:0.34.0-alpine

WORKDIR /opt/web

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN shards install --production
RUN shards build --release --no-debug --static {{#build_opts}} {{{.}}} {{/build_opts}}
