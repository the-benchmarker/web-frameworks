package main

import (
	"bytes"
	"log"

	"github.com/valyala/fasthttp"
)

var (
	strSlash          = []byte("/")
	strSlashUserSlash = []byte("/user/")
	strSlashUser      = []byte("/user")
)

func handler(ctx *fasthttp.RequestCtx) {
	method := string(ctx.Method())
	path := ctx.Request.RequestURI()

	switch method {
	case fasthttp.MethodGet:
		if bytes.HasPrefix(path, strSlashUserSlash) {
			id := path[len(strSlashUserSlash):]
			ctx.SetBody(id)

			return
		} else if bytes.Equal(path, strSlash) {
			return
		}

	case fasthttp.MethodPost:
		if bytes.Equal(path, strSlashUser) {
			return
		}
	}

	ctx.SetStatusCode(fasthttp.StatusNotFound)
}

func main() {
	log.Fatal(fasthttp.ListenAndServe(":3000", handler))
}
