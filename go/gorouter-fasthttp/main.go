package main

import (
	"github.com/valyala/fasthttp"
	"github.com/vardius/gorouter/v4"
	"github.com/vardius/gorouter/v4/context"
)

func index(ctx *fasthttp.RequestCtx) {
	ctx.SetBodyString("")
}

func user(ctx *fasthttp.RequestCtx) {
	ctx.SetBodyString("")
}

func userID(ctx *fasthttp.RequestCtx) {
	params := ctx.UserValue("params").(context.Params)
	ctx.SetBodyString(params.Value("id"))
}

func main() {
	router := gorouter.NewFastHTTPRouter()

	router.GET("/", index)
	router.GET("/user/{id:[0-9]+}", userID)
	router.POST("/user", user)

	fasthttp.ListenAndServe(":3000", router.HandleFastHTTP)
}
