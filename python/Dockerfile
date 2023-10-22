FROM python:3.12-slim

WORKDIR /usr/src/app

{{#build_deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -y update
{{/build_deps.length}}

{{#build_deps}}
  RUN apt-get -y install {{{.}}}
{{/build_deps}}

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

CMD {{command}}
