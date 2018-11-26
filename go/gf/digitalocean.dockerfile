# FROM ubuntu:18.10

FROM golang:1.11.2

# RUN apt-get -qq update
# RUN apt-get -qy install git
# RUN apt-get -qy install golang

# # we want go >= 1.11
# # ubuntu cosmic contains only go 1.10
# RUN apt-get -qy install software-properties-common
# RUN add-apt-repository -y ppa:longsleep/golang-backports
# RUN apt-get -qy install golang

WORKDIR /go/src/app

COPY main.go ./
COPY go.mod ./

ENV GO111MODULE=on
ENV CGO_ENABLED=0 
ENV GOOS=linux 
ENV GOARCH=amd64

RUN go build .

RUN mkdir -p /usr/src/app
RUN mv main /usr/src/app/server
RUN chmod +x /usr/src/app/server
