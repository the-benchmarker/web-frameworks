FROM golang:1.10

ENV CGO_ENABLED=0
ENV GOOS=linux
ENV GOARCH=amd64

WORKDIR /go/src/app
COPY . ./

RUN go get -u github.com/revel/cmd/revel
CMD revel build -a app -t target -m prod -v
