FROM gradle:jdk11

WORKDIR /usr/src/app

USER root
RUN chown -R gradle /usr/src/app
USER gradle

COPY . ./

RUN gradle build

FROM openjdk:11-jre-slim

WORKDIR /usr/src/app

COPY --from=0 /usr/src/app/build/libs/my-application.jar /usr/src/app/build/libs/my-application.jar

CMD java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar /usr/src/app/build/libs/my-application.jar

