FROM node:14.17-alpine

WORKDIR /usr/src/app

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}
{{^files}}
  COPY app.js package.json ./
{{/files}}

ENV NODE_ENV production

{{#bin_deps}}
  RUN apk add {{{.}}}
{{/deps}}

{{#files}}
  COPY {{source}} {{target}}
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
<<<<<<< HEAD
{{/bootstrap}}
=======
{{/before_command}}
>>>>>>> upstream/master

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#fixes}}
  RUN {{{.}}}
{{/fixes}}

CMD {{{command}}}