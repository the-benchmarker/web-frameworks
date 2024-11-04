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

CMD {{{command}}}
