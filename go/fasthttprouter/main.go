package main

import (
	"fmt"

	"github.com/buaazp/fasthttprouter"
	"github.com/valyala/fasthttp"
)

func main() {
	router := fasthttprouter.New()
	router.GET("/", func(ctx *fasthttp.RequestCtx) {
		fmt.Fprint(ctx, "")
	})
	router.GET("/user/:id", func(ctx *fasthttp.RequestCtx) {
		fmt.Fprintf(ctx, "%s", ctx.UserValue("id"))
	})
	router.POST("/user", func(ctx *fasthttp.RequestCtx) {
		fmt.Fprintf(ctx, "")
	})

	fasthttp.ListenAndServe(":3000", router.Handler)
}
