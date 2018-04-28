FROM golang

WORKDIR /usr/src/app
COPY main.go .

RUN go get -u github.com/gin-gonic/gin
RUN go build main.go

EXPOSE 3000
CMD [ "./main" ]
