FROM jrei/crystal-alpine

# RUN apt-get -y install libssl-dev
# RUN apt-get -y install libxml2-dev
# RUN apt-get -y install libyaml-dev
# RUN apt-get -y install libgmp-dev   
# RUN apt-get -y install libreadline-dev


WORKDIR /usr/src/app

COPY src src
COPY shard.yml ./

ENV shards install
RUN shards build --release --no-debug --static

RUN cp bin/server .
