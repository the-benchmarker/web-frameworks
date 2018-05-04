package main

import (
	"fmt"

	"github.com/buaazp/fasthttprouter"
	"github.com/valyala/fasthttp"
)

func Index(ctx *fasthttp.RequestCtx) {
	fmt.Fprint(ctx, "")
}

func User(ctx *fasthttp.RequestCtx) {
	fmt.Fprintf(ctx, "%s", ctx.UserValue("id"))
}

func Register(ctx *fasthttp.RequestCtx) {
	fmt.Fprintf(ctx, "")
}


func main() {
	router := fasthttprouter.New()
	router.GET("/", Index)
	router.GET("/user/:id", User)
	router.POST("/user", Register)

	fasthttp.ListenAndServe(":3000", router.Handler)
}
