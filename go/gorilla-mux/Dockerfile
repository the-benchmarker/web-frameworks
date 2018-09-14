FROM golang

WORKDIR /go/src/app
COPY main.go ./

RUN go get -u github.com/kardianos/govendor
RUN govendor init
RUN govendor fetch github.com/gorilla/mux@v1.6.2
RUN go build main.go

EXPOSE 3000
CMD [ "./main" ]
