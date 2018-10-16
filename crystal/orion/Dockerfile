FROM crystallang/crystal

WORKDIR /usr/src/app

COPY shard.yml server.cr ./

RUN shards build --release --no-debug

EXPOSE 3000
CMD bin/server
