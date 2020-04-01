package main

import (
	"github.com/valyala/fasthttp"
	"github.com/vardius/gorouter/v4"
	"github.com/vardius/gorouter/v4/context"
)

func index(ctx *fasthttp.RequestCtx) {}

func user(ctx *fasthttp.RequestCtx) {}

func userID(ctx *fasthttp.RequestCtx) {
	params := ctx.UserValue("params").(context.Params)
	ctx.SetBodyString(params.Value("id"))
}

func main() {
	router := gorouter.NewFastHTTPRouter()

	router.GET("/", index)
	router.GET("/user/{id}", userID)
	router.POST("/user", user)

	fasthttp.ListenAndServe(":3000", router.HandleFastHTTP)
}
