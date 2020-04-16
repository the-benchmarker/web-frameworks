FROM python:3.8-alpine

RUN apk add build-base

WORKDIR /opt/web

{{#deps}}
  RUN apk add {{{.}}}
{{/deps}}

{{#alpine-deps}}
  RUN apk add {{{.}}}
{{/alpine-deps}}

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}

{{#fixes}}
  RUN {{{.}}}
{{/fixes}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

{{#after_command}}
  RUN {{{.}}}
{{/after_command}}

CMD {{command}}
