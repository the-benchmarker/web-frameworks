FROM alpine:latest

RUN apk add --no-cache git cmake g++ make

ADD . /service

WORKDIR /service/utility

RUN ./install-oatpp-modules.sh

WORKDIR /service/build

RUN cmake ..
RUN make

EXPOSE 3000 3000

ENTRYPOINT ["./oatpp_app-exe"]
