FROM gradle:5.4-jdk8

WORKDIR /usr/src/app

COPY resources resources
COPY src src

COPY build.gradle build.gradle
COPY gradle.properties gradle.properties
COPY settings.gradle settings.gradle

USER root
RUN chown -R gradle /usr/src/app
USER gradle

RUN gradle build
RUN mv /usr/src/app/build/libs/my-application.jar .

EXPOSE 3000

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "my-application.jar"]

