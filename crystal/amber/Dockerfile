FROM crystallang/crystal:0.30.0

WORKDIR /usr/src/app

COPY shard.yml ./
COPY src src
COPY config config

RUN shards build --release --no-debug

CMD bin/server
