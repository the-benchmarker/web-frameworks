FROM crystallang/crystal:0.33.0

WORKDIR /usr/src/app

COPY . ./

RUN shards install --production
RUN shards build --release --no-debug

# TODO: find a way to add compilation options