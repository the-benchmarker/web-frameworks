FROM hseeberger/scala-sbt:11.0.7_1.3.12_2.13.2

WORKDIR /usr/src/app

COPY . ./

RUN sbt universal:packageZipTarball

FROM openjdk:11-jre

WORKDIR /usr/src/app

COPY --from=0 /usr/src/app/target target

RUN tar -xvzf target/universal/*.tgz

CMD {{#command}} {{{.}}} {{/command}}
