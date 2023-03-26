{{#provider.default.image-version}}
  FROM sbtscala/scala-sbt:eclipse-temurin-jammy-11.0.17_8_1.8.2_{{#provider.default.image-version}}{{{.}}}{{/provider.default.image-version}} AS build
{{/provider.default.image-version}}
{{^provider.default.image-version}}
  FROM sbtscala/scala-sbt:eclipse-temurin-jammy-11.0.17_8_1.8.2_3.2.2 AS build
{{/provider.default.image-version}}


WORKDIR /usr/src/app

COPY . ./

RUN sbt universal:packageZipTarball

FROM eclipse-temurin:17.0.6_10-jre-jammy

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/target target

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

CMD server-0.1.0-SNAPSHOT/bin/server
