FROM gradle:jdk25 AS build

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN gradle build



FROM eclipse-temurin:25.0.3_9-jre

WORKDIR /usr/src/app

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends curl && \
  rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/src/app/build/libs/server.jar /usr/src/app/build/libs/server.jar

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT ["java", "-server", "-jar", "/usr/src/app/build/libs/server.jar"]
