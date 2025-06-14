{{#language.image}}
  FROM sbtscala/scala-sbt:eclipse-temurin-21.0.5_11_1.10.6_{{{.}}} AS build
{{/language.image}}
{{^language.image}}
  FROM sbtscala/scala-sbt:eclipse-temurin-21.0.5_11_1.10.6_3.6.2 AS build
{{/language.image}}

WORKDIR /usr/src/app

{{#files}}
COPY '{{source}}' '{{target}}'
{{/files}}

RUN sbt universal:packageZipTarball

FROM eclipse-temurin:21.0.7_6-jre

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/target target

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

RUN apt-get -qq update
RUN apt-get -qy install curl

ENTRYPOINT {{command}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1
