FROM crystallang/crystal:1.11

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN apt-get -qq update

{{#build_deps}}
  RUN apt-get -qy install {{{.}}}
{{/build_deps}}

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
