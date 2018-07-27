FROM node

RUN npm -g install pm2

WORKDIR /usr/src/app

COPY app.js package.json ./

RUN npm install

ENV NODE_ENV production

CMD pm2-runtime start app.js -i $(nproc)
