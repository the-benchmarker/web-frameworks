FROM ruby:{{language.version}}-slim

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    build-essential \
    libjemalloc2 \
    curl && \
  {{#build_deps.length}}
  apt-get -y install {{#build_deps}}{{.}} {{/build_deps}} && \
  {{/build_deps.length}}
  rm -rf /var/lib/apt/lists/*

ENV BUNDLE_PATH="/usr/local/bundle" \
  LD_PRELOAD=libjemalloc.so.2

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN --mount=type=cache,id=bundler-gems-{{language.version}},target=${BUNDLE_PATH}/cache \
  bundle install

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
