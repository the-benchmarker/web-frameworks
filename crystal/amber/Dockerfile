FROM crystallang/crystal

WORKDIR /usr/src/app
COPY . .

RUN shards install
RUN shards build --release --no-debug

CMD bin/amber
