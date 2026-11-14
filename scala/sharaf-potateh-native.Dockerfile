FROM sbtscala/scala-sbt:eclipse-temurin-25.0.5_8_1.x AS build

RUN apt-get update && apt-get install -y --no-install-recommends clang gcc && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /usr/src/app
{{#files}}
COPY {{source}} {{target}}
{{/files}}
RUN sbt -batch nativeLink

FROM ubuntu:26.04
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgc1 libunwind8 libstdc++6 curl && rm -rf /var/lib/apt/lists/*
COPY --from=build /usr/src/app/target/native/server /usr/local/bin/sharaf-potateh
EXPOSE 3000
HEALTHCHECK CMD curl --fail --silent http://127.0.0.1:3000/ || exit 1
ENTRYPOINT {{{command}}}
