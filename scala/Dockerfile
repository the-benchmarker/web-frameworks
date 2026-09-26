FROM sbtscala/scala-sbt:eclipse-temurin-25.0.4_7_2.x AS build

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN sbt "Universal / packageZipTarball"



FROM eclipse-temurin:25.0.4_7-jre

WORKDIR /usr/src/app

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends curl && \
  rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/src/app/target target

# sbt 1 and sbt 2 place the packaged application in different target directories.
RUN tar -xvzf "$(find target -name server-0.1.0-SNAPSHOT.tgz)"

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
