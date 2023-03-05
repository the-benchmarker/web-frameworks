FROM gradle:7-jdk17 AS build

WORKDIR /usr/src/app

USER root
RUN chown -R gradle /usr/src/app
USER gradle

COPY . ./

RUN gradle build

FROM eclipse-temurin:17-jre

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/build/libs/server.jar /usr/src/app/build/libs/server.jar

CMD {{command}}

