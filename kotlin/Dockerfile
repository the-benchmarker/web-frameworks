FROM gradle:jdk11

WORKDIR /usr/src/app

USER root
RUN chown -R gradle .
USER gradle

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN gradle build
{{/build}}

FROM openjdk:11-jre-slim

WORKDIR /opt/web

COPY --from=0 /usr/src/app/build/libs/my-application.jar build/libs/my-application.jar

RUN ln -sfv /usr/local/openjdk-11/bin/java /usr/bin/java

CMD /usr/bin/java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar /opt/web/build/libs/my-application.jar

