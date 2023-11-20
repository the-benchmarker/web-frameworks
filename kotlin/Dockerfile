FROM gradle:jdk21 AS build

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN gradle build

FROM eclipse-temurin:21.0.1_12-jre-jammy

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/build/libs/server.jar /usr/src/app/build/libs/server.jar

CMD java -server -jar /usr/src/app/build/libs/server.jar

