{{#language.version}}
  FROM python:{{{.}}}-slim
{{/language.version}}
{{^language.version}}
  FROM python:3.12-slim
{{/language.version}}

WORKDIR /usr/src/app

{{#build_deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -y update

  {{#build_deps}}
    RUN apt-get -y install {{{.}}}
  {{/build_deps}}

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

RUN pip install -r requirements.txt

ENTRYPOINT {{command}}
