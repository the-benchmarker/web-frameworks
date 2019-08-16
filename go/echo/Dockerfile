FROM golang:1.13beta1 AS buildenv

ENV CGO_ENABLED=0
ENV GOOS=linux
ENV GOARCH=amd64

WORKDIR /go/src/app
ADD main.go go.mod ./

RUN go get
RUN go build -a -ldflags '-extldflags "-static"' -o /go/bin/app ./

FROM scratch
COPY --from=buildenv /go/bin/app /go/bin/app
CMD ["/go/bin/app"]
