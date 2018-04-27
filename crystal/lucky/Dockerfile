FROM crystallang/crystal

WORKDIR /usr/src/app

COPY .postcssrc.yml Procfile Procfile.dev brunch-config.js bs-config.js ./
COPY package.json shard.yml tasks.cr ./
COPY config config
COPY db db
COPY public public
COPY spec spec
COPY src src
COPY static static
COPY tasks tasks

RUN echo '{}'  > public/manifest.json

RUN shards build --release --no-debug

ENV LUCKY_ENV production

EXPOSE 3000

CMD bin/server
