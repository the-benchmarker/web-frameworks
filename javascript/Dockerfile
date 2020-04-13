FROM node:13.12-alpine

WORKDIR /usr/src/app

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}

{{#after_command}}
  RUN {{{.}}}
{{/after_command}}

RUN npm install

{{#environment}}
  ENV {{{.}}}
{{/environment}}

CMD {{{command}}}
