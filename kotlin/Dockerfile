FROM gradle:8-jdk17 AS build

WORKDIR /usr/src/app

COPY . ./

RUN gradle build

FROM openjdk:20-ea-17-slim

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/build/libs/server.jar /usr/src/app/build/libs/server.jar

CMD java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AlwaysPreTouch -jar /usr/src/app/build/libs/server.jar

