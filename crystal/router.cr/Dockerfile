FROM crystallang/crystal:0.30

WORKDIR /usr/src/app

COPY shard.yml ./
COPY src src

RUN shards build --release --no-debug

CMD bin/server
