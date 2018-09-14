FROM golang

WORKDIR /go/src/app
COPY main.go ./

RUN go get -u github.com/kardianos/govendor
RUN govendor init
RUN govendor fetch github.com/buaazp/fasthttprouter
RUN govendor fetch github.com/valyala/fasthttp
RUN go build main.go

EXPOSE 3000
CMD [ "./main" ]
