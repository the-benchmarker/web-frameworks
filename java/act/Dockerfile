FROM maven:3.6-jdk-8 as maven
WORKDIR /act
COPY pom.xml pom.xml
COPY src src
RUN mvn clean package -q

FROM openjdk:11-jre-slim
WORKDIR /act
COPY --from=maven /act/target/dist/*.tar.gz app.tar.gz
RUN tar xzf app.tar.gz

EXPOSE 3000

CMD ["/act/run"]
