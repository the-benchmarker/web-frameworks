FROM crystallang/crystal:0.30.1

WORKDIR /usr/src/app

COPY src src
COPY shard.yml ./
COPY athena.yml ./

RUN shards build --production --no-debug

ENV ATHENA_ENV test

CMD bin/server
