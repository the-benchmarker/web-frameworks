FROM golang:1.14-alpine

ENV CGO_ENABLED=0
ENV GOOS=linux
ENV GOARCH=amd64

RUN apk add git
RUN go get -u github.com/revel/cmd/revel



WORKDIR /go/src

COPY . ./


# RUN go get

# RUN go build -a -ldflags '-extldflags "-static"' -o /go/bin/app ./

# FROM alpine
# COPY --from=0 /go/bin/app /go/bin/app
# CMD cd /go/bin && ./app
