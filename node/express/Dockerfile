FROM node

RUN npm -g install pm2

WORKDIR /usr/src/app
COPY . .

RUN npm install

ENV NODE_ENV production
