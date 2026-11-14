FROM eclipse-temurin:26.0.2_10-jdk-noble AS build

WORKDIR /usr/src/app

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends curl && \
  curl -fsSL https://raw.githubusercontent.com/paulp/sbt-extras/master/sbt -o /usr/local/bin/sbt && \
  chmod +x /usr/local/bin/sbt && \
  rm -rf /var/lib/apt/lists/*

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN sbt -sbt-version 1.13.1 "Universal / packageZipTarball"


FROM eclipse-temurin:26.0.2_10-jre-noble

WORKDIR /usr/src/app

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends curl && \
  rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/src/app/target target

# sbt 1 and sbt 2 place the packaged application in different target directories.
RUN tar -xvzf "$(find target -name server-0.1.0-SNAPSHOT.tgz)"

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
