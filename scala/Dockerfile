FROM hseeberger/scala-sbt:11.0.10_1.5.2_2.13.6 AS build

WORKDIR /usr/src/app

COPY . ./

RUN sbt universal:packageZipTarball

FROM openjdk:11-jre

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/target target

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

CMD server-0.1.0-SNAPSHOT/bin/server
