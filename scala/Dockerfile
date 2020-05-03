FROM hseeberger/scala-sbt:11.0.7_1.3.12_2.13.2 AS build

WORKDIR /usr/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN sbt universal:packageZipTarball

FROM openjdk:11-jre

WORKDIR /usr/src/app

{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

CMD server-0.1.0-SNAPSHOT/bin/server
