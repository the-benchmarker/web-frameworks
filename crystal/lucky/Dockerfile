FROM crystallang/crystal:0.31.1

WORKDIR /usr/src/app

COPY shard.yml ./
COPY public public
COPY src src
COPY config config

RUN echo '{}'  > public/manifest.json
RUN echo '{}'  > public/mix-manifest.json

RUN shards build --release --no-debug

ENV LUCKY_ENV production

EXPOSE 3000

CMD bin/server
