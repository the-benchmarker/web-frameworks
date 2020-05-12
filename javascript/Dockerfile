FROM node:13.14-alpine

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

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#alpine-fixes}}
  RUN {{{.}}}
{{/alpine-fixes}}

RUN npm install

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#command}}
  CMD {{{.}}}
{{/command}}

{{^command}}
  CMD pm2-runtime start app.js -i $(nproc)
{{/command}}
