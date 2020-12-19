FROM hseeberger/scala-sbt:11.0.9.1_1.4.4_2.13.4 AS build

WORKDIR /usr/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

{{#files}}
  COPY {{.}} {{{.}}}
{{/files}}

RUN sbt universal:packageZipTarball

FROM openjdk:11-jre

WORKDIR /opt/web

{{#files}}
  COPY {{{.}}} {{{.}}}
{{/files}}
  
{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

CMD server-0.1.0-SNAPSHOT/bin/server
