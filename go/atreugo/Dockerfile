FROM golang:1.12.7

WORKDIR /go/src/app
COPY main.go go.mod ./

RUN go build .

EXPOSE 3000
CMD [ "./main" ]
