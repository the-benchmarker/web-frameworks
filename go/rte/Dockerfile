FROM golang:1.12 AS builder

WORKDIR /go/src/app
COPY main.go go.mod ./

ENV GO111MODULE=on CGO_ENABLED=0
RUN go build .

FROM scratch

COPY --from=builder /go/src/app/main /main

ENV SERVER_ADDRESS ":3000"

EXPOSE 3000

CMD [ "/main" ]
