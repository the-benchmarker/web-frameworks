FROM golang:1.11.5

WORKDIR /go/src/app
COPY main.go go.mod ./

ENV GO111MODULE=on
RUN go build main.go

EXPOSE 3000
CMD [ "./main" ]
