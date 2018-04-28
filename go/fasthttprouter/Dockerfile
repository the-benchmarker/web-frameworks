FROM golang

WORKDIR /usr/src/app
COPY main.go ./

RUN go get -u github.com/buaazp/fasthttprouter
RUN go get -u github.com/valyala/fasthttp
RUN go build main.go

EXPOSE 3000
CMD [ "./main" ]
