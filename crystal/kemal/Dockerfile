FROM crystallang/crystal:0.26.1

WORKDIR /usr/src/app

COPY src src
COPY shard.yml ./

RUN shards build --release --no-debug

CMD for i in $(seq 1 $(nproc --all)); do bin/server & done
