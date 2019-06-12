FROM golang:1.12.6 AS buildenv

ENV GO111MODULE=on
ENV CGO_ENABLED=0

WORKDIR /go/src/app
ADD . ./

RUN go get
RUN go build -a -o /go/bin/app ./

FROM scratch
COPY --from=buildenv /go/bin/app /go/bin/app
CMD ["/go/bin/app"]
