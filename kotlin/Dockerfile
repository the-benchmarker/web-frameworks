FROM gradle:jdk11 As build

WORKDIR /usr/src/app

USER root
RUN chown -R gradle /usr/src/app
USER gradle

COPY . ./

RUN gradle build

FROM openjdk:11.0.16-jre-slim

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/build/libs/server.jar /usr/src/app/build/libs/server.jar

CMD java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AlwaysPreTouch -jar /usr/src/app/build/libs/server.jar

