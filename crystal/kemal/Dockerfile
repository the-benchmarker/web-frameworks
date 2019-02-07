FROM crystallang/crystal:0.27.2

WORKDIR /usr/src/app

COPY src src
COPY shard.yml ./

RUN shards build --release --no-debug

CMD bin/server
