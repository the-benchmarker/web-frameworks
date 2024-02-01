FROM node:20-slim

WORKDIR /usr/src/app

RUN apt-get -qq update

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#deps}}
  RUN apt-get -qy install {{{.}}}
{{/deps}}

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
