FROM crystallang/crystal:0.26.1

WORKDIR /usr/src/app

COPY shard.yml app.cr ./

RUN shards build --release --no-debug

CMD APP_ENV=production bin/server
