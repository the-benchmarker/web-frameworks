FROM oven/bun:1.2-slim

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN apt-get -qq update

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

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
