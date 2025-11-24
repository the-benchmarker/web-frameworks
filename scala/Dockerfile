FROM sbtscala/scala-sbt:eclipse-temurin-25.0.1_8_1.11.7_3.7.4 AS build

WORKDIR /usr/src/app

{{#files}}
COPY '{{source}}' '{{target}}'
{{/files}}

RUN sbt universal:packageZipTarball

FROM eclipse-temurin:25-jre

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/target target

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

RUN apt-get -qq update
RUN apt-get -qy install curl

ENTRYPOINT {{command}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1
