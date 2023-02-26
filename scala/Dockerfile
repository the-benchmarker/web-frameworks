FROM hseeberger/scala-sbt:11.0.13_1.6.1_2.13.8 AS build

WORKDIR /usr/src/app

COPY . ./

RUN sbt universal:packageZipTarball

FROM openjdk:11.0.16-jre

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/target target

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

CMD {{command}}
