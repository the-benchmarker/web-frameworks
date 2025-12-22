{{#language.java.version}}
  FROM gradle:jdk{{{.}}} AS build
{{/language.java.version}}
{{^language.java.version}}
  FROM gradle:jdk25 AS build
{{/language.java.version}}

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN gradle build

{{#language.java.version}}
  FROM eclipse-temurin:{{{.}}}-jre
{{/language.java.version}}
{{^language.java.version}}
  FROM eclipse-temurin:25-jre
{{/language.java.version}}

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/build/libs/server.jar /usr/src/app/build/libs/server.jar

RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT ["java", "-server", "-jar", "/usr/src/app/build/libs/server.jar"]
