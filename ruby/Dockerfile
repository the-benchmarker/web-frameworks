FROM ruby:3.3-slim

WORKDIR /usr/src/app

RUN apt-get -qq update

RUN apt-get -qy install --no-install-recommends build-essential

RUN apt-get install -y --no-install-recommends libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

RUN bundle install

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK --interval=1m --timeout=60s --retries=3 CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
