{{#language.version}}
  FROM python:{{{.}}}-slim
{{/language.version}}
{{^language.version}}
  FROM python:3.14-slim
{{/language.version}}

WORKDIR /usr/src/app

{{#build_deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -qq update && \
  {{#build_deps}}
      apt-get -qy install --no-install-recommends {{{.}}} && \
  {{/build_deps}}
      apt-get clean && rm -rf /var/lib/apt/lists/*

{{/build_deps.length}}

{{#files}}
  COPY {{source}} {{target}}
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

RUN pip install --no-cache-dir .

RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
