FROM python:3.8-alpine

RUN ln -sfv /usr/local/bin/python /usr/bin/python3
RUN ln -sfv /usr/local/bin/pip /usr/bin/pip

RUN apk add build-base

WORKDIR /opt/web

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

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

CMD {{#command}}{{{.}}}{{/command}}
