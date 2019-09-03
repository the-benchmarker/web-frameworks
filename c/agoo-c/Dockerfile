FROM gcc:9.2

ENV DEBIAN_FRONTEND=noninteractive

WORKDIR /usr/src/app

RUN apt-get -qq update && \
    apt-get -qy install cmake 

COPY . ./



FROM debian

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update

COPY --from=0 /usr/src/app/simple /usr/bin/simple

