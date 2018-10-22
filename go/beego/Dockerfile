FROM golang:1.11.1

WORKDIR /go/src/app
COPY main.go ./

RUN go get -u github.com/kardianos/govendor
RUN govendor init
RUN govendor fetch github.com/astaxie/beego@v1.10.0
RUN go build main.go

EXPOSE 3000
CMD [ "./main" ]
