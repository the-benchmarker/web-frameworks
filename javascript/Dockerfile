FROM node:12.13-alpine

{{#build_deps}}
  RUN apk add {{{.}}}
{{/build_deps}}

RUN npm -g install pm2

WORKDIR /usr/src/app

COPY app.js package.json ./

{{#bin_deps}}
  RUN apk add {{{.}}}
{{/bin_deps}}

RUN npm install

ENV NODE_ENV production

CMD pm2-runtime start app.js -i $(nproc)
