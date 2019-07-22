FROM node:12.6.0

WORKDIR /usr/src/app

COPY app.js package.json ./

RUN apk update && apk upgrade && \
    apk add --no-cache bash git openssh

RUN npm install

CMD ["node", "app.js"]
