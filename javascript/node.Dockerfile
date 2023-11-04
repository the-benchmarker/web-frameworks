FROM node:lts-slim

WORKDIR /usr/src/app

RUN apt-get -qq update

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#deps}}
  RUN apt-get -qy install add {{{.}}}
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

RUN npm install -g npm@10

CMD {{{command}}}
