FROM node:14.16-alpine

WORKDIR /usr/src/app

{{#files}}
  COPY {{source}} {{target}}
{{/files}}

RUN apk add build-base

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{{command}}}