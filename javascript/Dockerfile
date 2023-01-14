{{#image}}
  FROM {{{.}}}
{{/image}}

{{^image}}
  FROM node:19.4-alpine
{{/image}}



{{#build_deps}}
  RUN apk add {{{.}}}
{{/build_deps}}

RUN npm -g install pm2

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
{{/bin_deps}}

{{#fixes}}
  RUN {{{.}}}
{{/fixes}}

RUN npm install

{{#before_command}}
  RUN {{{.}}}
{{/before_command}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

{{#command}}
  CMD {{{.}}}
{{/command}}

{{^command}}
  CMD pm2-runtime start app.js -i $(nproc)
{{/command}}
