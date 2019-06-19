FROM maven:3.6-jdk-8 as maven
WORKDIR /spring-boot
COPY pom.xml pom.xml
COPY src src
RUN mvn clean package -q

FROM openjdk:11-jre-slim
WORKDIR /spring-boot
COPY --from=maven /spring-boot/target/benchmark-0.0.1-SNAPSHOT.jar app.jar

EXPOSE 3000

CMD ["java", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "app.jar"]

