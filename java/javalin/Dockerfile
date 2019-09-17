FROM maven:3.6-jdk-14 as maven
WORKDIR /javalin
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:11-jre-slim
WORKDIR /javalin
COPY --from=maven /javalin/target/benchmark-0.0.1-SNAPSHOT-jar-with-dependencies.jar app.jar

EXPOSE 3000

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-cp", "app.jar", "benchmark.javalin.Bench", "profiles=production"]
