FROM gradle:8-jdk17 AS build

WORKDIR /usr/src/app

COPY . ./

RUN gradle build

FROM eclipse-temurin:17.0.6_10-jre

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/build/libs/server.jar /usr/src/app/build/libs/server.jar

CMD {{command}}

