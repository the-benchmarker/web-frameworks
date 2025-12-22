FROM ruby:3.4-slim

WORKDIR /usr/src/app

RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends build-essential libjemalloc2 curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

ENV LD_PRELOAD=libjemalloc.so.2

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#build_deps}}
  RUN apt-get -qq update && apt-get -qy install --no-install-recommends {{{.}}} && apt-get clean && rm -rf /var/lib/apt/lists/*
{{/build_deps}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

RUN bundle install

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

ENTRYPOINT {{{command}}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1
