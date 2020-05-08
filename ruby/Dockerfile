FROM ruby:2.7-alpine

WORKDIR /opt/web

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}

RUN apk add build-base

{{#alpine-deps}}
  RUN apk add {{{.}}} {{{.}}}
{{/alpine-deps}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#after_command}}
  RUN {{{.}}}
{{/after_command}}

{{#command}}
  CMD {{{command}}}
{{/command}}
