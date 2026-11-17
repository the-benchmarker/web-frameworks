FROM sbtscala/scala-sbt:eclipse-temurin-25_2.x AS build
WORKDIR /usr/src/app
{{#files}}
COPY {{source}} {{target}}
{{/files}}
RUN sbt -batch assembly

FROM eclipse-temurin:25-jre-alpine
WORKDIR /app
RUN apk add --no-cache curl
COPY --from=build /usr/src/app/target/app.jar app.jar
EXPOSE 3000
HEALTHCHECK CMD curl --fail --silent http://127.0.0.1:3000/ || exit 1
ENTRYPOINT {{{command}}}
