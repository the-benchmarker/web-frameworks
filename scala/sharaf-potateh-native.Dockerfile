FROM eclipse-temurin:21.0.11_10-jdk-noble AS build

RUN apt-get update && apt-get install -y --no-install-recommends \
    clang libunwind-dev libgc-dev curl ca-certificates && \
    rm -rf /var/lib/apt/lists/*
RUN curl -fL https://github.com/sake92/deder/releases/download/v0.22.2/deder-client.jar \
    -o /opt/deder-client.jar

WORKDIR /usr/src/app
{{#files}}
COPY {{source}} {{target}}
{{/files}}
RUN java -jar /opt/deder-client.jar exec -t nativeLink

FROM ubuntu:26.04
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgc1 libunwind8 libstdc++6 curl && rm -rf /var/lib/apt/lists/*
COPY --from=build /usr/src/app/.deder/out/server/nativeLink/server /usr/local/bin/sharaf-potateh
EXPOSE 3000
HEALTHCHECK CMD curl --fail --silent http://127.0.0.1:3000/ || exit 1
ENTRYPOINT {{{command}}}
