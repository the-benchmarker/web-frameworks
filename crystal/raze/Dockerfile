FROM crystallang/crystal:0.30.1

WORKDIR /usr/src/app

COPY shard.yml ./
COPY src src

RUN shards build --release --no-debug

EXPOSE 3000
CMD bin/server
