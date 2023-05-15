{{#provider.default.image-version}}
  FROM sbtscala/scala-sbt:eclipse-temurin-jammy-11.0.17_8_1.8.2_{{{.}}} AS build
{{/provider.default.image-version}}
{{^provider.default.image-version}}
  FROM sbtscala/scala-sbt:eclipse-temurin-jammy-11.0.17_8_1.8.2_3.2.2 AS build
{{/provider.default.image-version}}


WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN sbt universal:packageZipTarball

{{#provider.default.java-version}}
  FROM eclipse-temurin:{{{.}}}-jre-jammy
{{/provider.default.java-version}}
{{^provider.default.java-version}}
  FROM eclipse-temurin:17.0.7_7-jre-jammy
{{/provider.default.java-version}}

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/target target

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

CMD {{command}}
