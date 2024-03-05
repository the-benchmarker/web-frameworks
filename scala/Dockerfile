{{#language.image-version}}
  FROM sbtscala/scala-sbt:eclipse-temurin-jammy-21.0.2_13_1.9.9_{{{.}}} AS build
{{/language.image-version}}
{{^language.image-version}}
  FROM sbtscala/scala-sbt:eclipse-temurin-jammy-21.0.2_13_1.9.9_3.3.1 AS build
{{/language.image-version}}


WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN sbt universal:packageZipTarball

{{#language.java-version}}
  FROM eclipse-temurin:{{{.}}}-jre-jammy
{{/language.java-version}}
{{^language.java-version}}
  FROM eclipse-temurin:21.0.2_13-jre-jammy
{{/language.java-version}}

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/target target

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

CMD {{command}}
