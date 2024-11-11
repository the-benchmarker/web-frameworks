FROM node:22-slim

WORKDIR /usr/src/app

RUN apt-get -qq update

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -qq update
  {{#deps}}
    RUN apt-get -qy install {{{.}}}
  {{/deps}}
{{/deps.length}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#fixes}}
  RUN {{{.}}}
{{/fixes}}

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
