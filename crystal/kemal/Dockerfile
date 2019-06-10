FROM crystallang/crystal:0.29.0

WORKDIR /usr/src/app

COPY src src
COPY shard.yml ./

RUN shards build --release --no-debug

CMD bin/server
