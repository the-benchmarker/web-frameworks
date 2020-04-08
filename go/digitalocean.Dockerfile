FROM golang:1.14

ENV CGO_ENABLED=0
ENV GOOS=linux
ENV GOARCH=amd64

WORKDIR /go/src/app
COPY . ./

{{#before_build}}
  RUN {{{.}}}
{{/before_build}}

RUN go get

RUN go build -a -ldflags '-extldflags "-static"' -o /go/bin/app ./
