FROM crystallang/crystal:0.29.0

WORKDIR /usr/src/app

COPY shard.yml ./
COPY src src

ENV BENCHMARK true

RUN shards build --release --no-debug

EXPOSE 3000
CMD bin/server
