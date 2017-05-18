FROM crystallang/crystal

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

COPY . /usr/src/app

RUN shards build --release

CMD ./bin/server_crystal_kemal
