FROM golang:1.12.8 AS buildenv

ENV CGO_ENABLED=0
ENV GOOS=linux
ENV GOARCH=amd64

WORKDIR /app
ADD main.go go.mod go.sum ./

RUN GOPROXY=https://proxy.golang.org go mod download
RUN go build -a -ldflags '-extldflags "-static"' -o /app/do ./

FROM scratch
COPY --from=buildenv /app/do /app/do
CMD ["/app/do"]
