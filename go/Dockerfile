{{#version}}
  FROM golang:{{version}}-alpine
{{/version}}
{{^version}}
  FROM golang:1.19-alpine
{{/version}}

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}



RUN apk add build-base {{#deps}}{{{.}}}{{/deps}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#static_files}}
  COPY '{{source}}' '/go/bin/{{target}}'
{{/static_files}}

CMD {{{command}}}
