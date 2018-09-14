FROM golang

WORKDIR /go/src/app
COPY main.go ./

RUN go get -u github.com/kardianos/govendor
RUN govendor init
RUN govendor fetch github.com/labstack/echo@v3.3.6
RUN go build main.go

EXPOSE 3000
CMD [ "./main" ]
