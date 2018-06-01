FROM maven:3.5.3-alpine

WORKDIR /opt/app

COPY pom.xml ./
COPY src src

RUN mvn clean package
RUN cp /opt/app/target/dist/*.tar.gz .
RUN tar xzf *.tar.gz

EXPOSE 3000

CMD /opt/app/run
