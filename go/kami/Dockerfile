FROM golang:1.12.5

WORKDIR /go/src/app
COPY main.go go.mod ./

ENV GO111MODULE=on
RUN go get
RUN go build .

ENV GOJI_BIND "0.0.0.0:3000"
EXPOSE 3000
CMD [ "./main" ]
