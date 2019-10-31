package main

import (
	"bytes"

	"github.com/valyala/fasthttp"
)

var (
	strGET            = []byte("GET")
	strPOST           = []byte("POST")
	strSlash          = []byte("/")
	strSlashUserSlash = []byte("/user/")
	strSlashUser      = []byte("/user")
)

func main() {
	fasthttp.ListenAndServe(":3000", func(ctx *fasthttp.RequestCtx) {
		method := ctx.Method()
		path := ctx.Request.RequestURI()

		if bytes.Equal(method, strGET) {
			if bytes.Equal(path, strSlash) {
				ctx.SetBodyString("")
				return
			} else if bytes.HasPrefix(path, strSlashUserSlash) {
				id := path[6:]
				ctx.SetBody(id)
				return
			}
		} else if bytes.Equal(method, strPOST) {
			if bytes.Equal(path, strSlashUser) {
				ctx.SetBodyString("")
				return
			}
		}

		ctx.SetStatusCode(fasthttp.StatusNotFound)
	})
}
