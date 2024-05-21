
  FROM sbtscala/scala-sbt:eclipse-temurin-jammy-22_36_1.9.9_3.4.2 AS build

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN sbt universal:packageZipTarball

  FROM eclipse-temurin:21.0.3_9-jre-jammy

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/target target

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

CMD {{command}}
