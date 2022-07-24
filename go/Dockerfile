{{#docker-image}}
  FROM {{{.}}}
{{/docker-image}}
{{^docker-image}}
  FROM golang:1.18-alpine
{{/docker-image}}

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
