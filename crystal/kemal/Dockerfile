FROM crystallang/crystal:0.30.1

WORKDIR /usr/src/app

COPY src src
COPY shard.yml ./

RUN shards build --release --no-debug

CMD bin/server
