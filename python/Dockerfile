{{#language.version}}
FROM python:{{{.}}}-slim
{{/language.version}}
{{^language.version}}
FROM python:3.15-slim
{{/language.version}}

WORKDIR /usr/src/app

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl && \
  {{#build_deps.length}}
  apt-get -y install {{#build_deps}}{{{.}}} {{/build_deps}} && \
  {{/build_deps.length}}
  rm -rf /var/lib/apt/lists/*

{{#files}}
  COPY {{source}} {{target}}
{{/files}}

RUN --mount=type=cache,id=pip-cache-3.14,target=/root/.cache/pip \
  pip install .

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
