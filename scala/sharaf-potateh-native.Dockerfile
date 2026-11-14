FROM sbtscala/scala-sbt:eclipse-temurin-alpine-25.0.4_7_1.13.0_3.10.0 AS build

RUN apk add --no-cache clang gcc

WORKDIR /usr/src/app
{{#files}}
COPY {{source}} {{target}}
{{/files}}
RUN sbt -batch nativeLink

FROM alpine:3.23
COPY --from=build /usr/src/app/target/native/server /usr/local/bin/sharaf-potateh
EXPOSE 3000
HEALTHCHECK CMD wget -q -O /dev/null http://127.0.0.1:3000/ || exit 1
ENTRYPOINT {{{command}}}
