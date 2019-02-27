FROM golang:1.12.0

WORKDIR /go/src/app
COPY main.go go.mod ./

RUN go build main.go

EXPOSE 3000
CMD [ "./main" ]
