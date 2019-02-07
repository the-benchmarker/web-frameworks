FROM crystallang/crystal:0.27.2

WORKDIR /usr/src/app

COPY .postcssrc.yml Procfile Procfile.dev brunch-config.js bs-config.js ./
COPY shard.yml tasks.cr ./
COPY db db
COPY public public
COPY src src
COPY static static
COPY config config
COPY tasks tasks

RUN echo '{}'  > public/manifest.json
RUN echo '{}'  > public/mix-manifest.json

RUN shards build --release --no-debug

ENV LUCKY_ENV production

EXPOSE 3000

CMD bin/server
