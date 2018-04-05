FROM crystallang/crystal

WORKDIR /usr/src/app
COPY . .

RUN mkdir -p public
RUN echo '{}'  > public/manifest.json

RUN shards install
RUN shards build --release --no-debug

ENV LUCKY_ENV production

EXPOSE 3000
CMD ./bin/server_crystal_lucky
