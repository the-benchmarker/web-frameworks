FROM golang:1.12.5

WORKDIR /go/src/app
COPY main.go go.mod ./

ENV CGO_ENABLED=0

RUN go get
RUN go build .

EXPOSE 3000
CMD [ "./main" ]
