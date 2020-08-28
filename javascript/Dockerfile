FROM node:14.9-alpine

{{#build_deps}}
  RUN apk add {{{.}}}
{{/build_deps}}

RUN npm -g install pm2

WORKDIR /usr/src/app

COPY app.js package.json ./

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}

{{#bin_deps}}
  RUN apk add {{{.}}}
{{/bin_deps}}

{{#fixes}}
  RUN {{{.}}}
{{/fixes}}

RUN npm install

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}


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
