FROM golang:1.13

ENV CGO_ENABLED=0
ENV GOOS=linux
ENV GOARCH=amd64

WORKDIR /go/src/app
COPY . ./

RUN go get
RUN go build -a -ldflags '-extldflags "-static"' -o /go/bin/app ./

FROM alpine
{{#environment}}
ENV {{{.}}}
{{/environment}}
COPY --from=0 /go/bin/app /go/bin/app
{{#files}}
COPY --from=0 /go/src/app/{{.}} /go/{{{.}}}
{{/files}}

WORKDIR /go
CMD /go/bin/app
