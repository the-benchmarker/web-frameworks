FROM golang:1.12.5

WORKDIR /go/src/app
COPY main.go go.mod ./

RUN go get
RUN go build .

EXPOSE 3000
CMD [ "./main" ]
