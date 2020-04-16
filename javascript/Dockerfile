FROM node:13.12-alpine

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

{{#alpine-fixes}}
  RUN {{{.}}}
{{/alpine-fixes}}

RUN npm install

{{#after_command}}
  RUN {{{.}}}
{{/after_command}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{{command}}}
