FROM maven:3.6-jdk-8 as maven
WORKDIR /rapidoid
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:11-jre-slim
WORKDIR /rapidoid
COPY --from=maven /rapidoid/target/benchmark-0.0.1-SNAPSHOT-jar-with-dependencies.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-cp", "app.jar", "benchmark.rapidoid.Main", "profiles=production"]
