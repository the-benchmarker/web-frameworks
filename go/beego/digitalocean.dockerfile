FROM golang:1.11.2

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
