FROM gradle
WORKDIR /usr/src/app

COPY . ./

RUN gradle build


FROM openjdk

WORKDIR /usr/src/app

COPY --from=0 /usr/src/app/build/distributions/micronaut-0.1.tar .
RUN tar -xvf micronaut-0.1.tar

CMD micronaut-0.1/bin/micronaut
