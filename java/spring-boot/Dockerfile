FROM maven:3.6-jdk-8

WORKDIR /usr/src/app

COPY pom.xml ./
COPY src src

RUN mvn clean package
RUN cp /usr/src/app/target/*.jar /usr/src/app/app.jar

EXPOSE 3000

CMD java -jar /usr/src/app/app.jar
