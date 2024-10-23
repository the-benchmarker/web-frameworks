FROM sbtscala/scala-sbt:eclipse-temurin-alpine-21.0.2_13_1.10.3_3.5.2 AS build

WORKDIR /usr/src/app

{{#files}}
COPY '{{source}}' '{{target}}'
{{/files}}

RUN sbt universal:packageZipTarball

FROM eclipse-temurin:21.0.4_7-jre-jammy

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/target target

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

CMD {{command}}
