FROM node:18.16-alpine

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#deps}}
  RUN apk add {{{.}}}
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
