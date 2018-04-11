FROM crystallang/crystal:0.24.2

WORKDIR /usr/src/app
COPY . .

RUN shards install
RUN shards build --release --no-debug

CMD bin/amber
