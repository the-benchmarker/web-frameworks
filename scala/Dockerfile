FROM sbtscala/scala-sbt:eclipse-temurin-focal-17.0.5_8_1.8.2_3.2.2 AS build

WORKDIR /usr/src/app

COPY . ./

RUN sbt universal:packageZipTarball

FROM openjdk:11.0.16-jre

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/target target

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

CMD server-0.1.0-SNAPSHOT/bin/server
