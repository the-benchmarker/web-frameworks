FROM golang:1.11.1

WORKDIR /go/src/app
COPY main.go ./

RUN go get -u github.com/kardianos/govendor
RUN govendor init
RUN govendor fetch github.com/buaazp/fasthttprouter@v0.1.1
RUN govendor fetch github.com/valyala/fasthttp@v20180529
RUN go build main.go

EXPOSE 3000
CMD [ "./main" ]
