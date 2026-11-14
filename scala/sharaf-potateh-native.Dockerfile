FROM eclipse-temurin:21-jdk-alpine AS build

RUN apk add --no-cache clang gcc curl
RUN curl -fL https://github.com/sake92/deder/releases/download/v0.22.2/deder-client.jar \
    -o /opt/deder-client.jar

WORKDIR /usr/src/app
{{#files}}
COPY {{source}} {{target}}
{{/files}}
RUN java -jar /opt/deder-client.jar exec -t nativeLink

FROM alpine:3.23
COPY --from=build /usr/src/app/.deder/out/server/nativeLink/server /usr/local/bin/sharaf-potateh
EXPOSE 3000
HEALTHCHECK CMD wget -q -O /dev/null http://127.0.0.1:3000/ || exit 1
ENTRYPOINT {{{command}}}
