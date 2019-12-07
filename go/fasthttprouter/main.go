package main

import (
	"github.com/buaazp/fasthttprouter"
	"github.com/valyala/fasthttp"
)

func main() {
	router := fasthttprouter.New()
	router.GET("/", func(ctx *fasthttp.RequestCtx) {
		ctx.SetBodyString("")
	})
	router.GET("/user/:id", func(ctx *fasthttp.RequestCtx) {
		ctx.SetBodyString(ctx.UserValue("id").(string))
	})
	router.POST("/user", func(ctx *fasthttp.RequestCtx) {
		ctx.SetBodyString("")
	})

	fasthttp.ListenAndServe(":3000", router.Handler)
}
