FROM golang:1.11

WORKDIR /go/src/app
COPY main.go ./

RUN go get -u github.com/kardianos/govendor
RUN govendor init
RUN govendor fetch github.com/gin-gonic/gin@v1.3.0
RUN go build main.go

EXPOSE 3000
CMD [ "./main" ]
