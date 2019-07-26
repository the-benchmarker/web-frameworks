FROM node:12.6.0

RUN npm -g install pm2

WORKDIR /usr/src/app

COPY app.js package.json ./

RUN apk update && apk add --no-cache git
RUN npm install
RUN ln -s /lib/libc.musl-x86_64.so.1 /lib/ld-linux-x86-64.so.2

ENV NODE_CLUSTER_SCHED_POLICY=rr
ENV NODE_ENV production

CMD pm2-runtime start app.js -i $(nproc)
