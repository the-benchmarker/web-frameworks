{{#language.image-version}}
  FROM sbtscala/scala-sbt:eclipse-temurin-jammy-11.0.20.1_1_1.9.6_{{{.}}} AS build
{{/language.image-version}}
{{^language.image-version}}
  FROM sbtscala/scala-sbt:eclipse-temurin-jammy-11.0.20.1_1_1.9.6_3.3.1 AS build
{{/languageimage-version}}


WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN sbt universal:packageZipTarball

{{#language.java-version}}
  FROM eclipse-temurin:{{{.}}}-jre-jammy
{{/language.java-version}}
{{^language.java-version}}
  FROM eclipse-temurin:17-jre-jammy
{{/language.java-version}}

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/target target

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

CMD {{command}}
