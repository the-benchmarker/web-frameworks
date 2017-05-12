FROM crystallang/crystal

RUN mkdir /app
WORKDIR /app

COPY . /app

RUN shards build --release

CMD ./bin/server_crystal_router_cr
