FROM gradle:jdk11 As build

WORKDIR /usr/src/app

USER root
RUN chown -R gradle .
USER gradle

COPY . ./

RUN gradle build

FROM openjdk:11-jre-slim

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/build/libs/server.jar /usr/src/app/build/libs/server.jar

CMD {{{command}}}

