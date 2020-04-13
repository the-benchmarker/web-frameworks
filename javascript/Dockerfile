FROM node:13.14-alpine

WORKDIR /usr/src/app

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

RUN npm install

{{#after_command}}
  RUN {{{.}}}
{{/after_command}}

ENV NODE_ENV production

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#command}}
  CMD {{{.}}}
{{/command}}

{{^command}}
  CMD pm2-runtime start app.js -i $(nproc)
{{/command}}
