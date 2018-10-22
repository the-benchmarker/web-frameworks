FROM golang:1.11.1

WORKDIR /go/src/app
COPY main.go ./

RUN go get -u github.com/kardianos/govendor
RUN govendor init
RUN govendor fetch github.com/kataras/muxie@v1.0.0
RUN go build main.go

EXPOSE 3000
CMD [ "./main" ]
