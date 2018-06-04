FROM maven

WORKDIR /usr/src/app

COPY pom.xml ./
COPY src src

RUN mvn clean package
RUN cp /usr/src/app/target/dist/*.tar.gz .
RUN tar xzf *.tar.gz

EXPOSE 3000

CMD /usr/src/app/run
