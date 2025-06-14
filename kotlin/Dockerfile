FROM gradle:jdk21 AS build

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN gradle build

FROM eclipse-temurin:21.0.7_6-jre

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/build/libs/server.jar /usr/src/app/build/libs/server.jar

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT java -server -jar /usr/src/app/build/libs/server.jar

