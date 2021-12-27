package main

import (
	"log"

	"github.com/valyala/fasthttp"
	"github.com/vardius/gorouter/v4"
	"github.com/vardius/gorouter/v4/context"
)

func showEmpty(ctx *fasthttp.RequestCtx) {}

func showID(ctx *fasthttp.RequestCtx) {
	params := ctx.UserValue("params").(context.Params)
	ctx.SetBodyString(params.Value("id"))
}

func main() {
	router := gorouter.NewFastHTTPRouter()

	router.GET("/", showEmpty)
	router.GET("/user/{id}", showID)
	router.POST("/user", showEmpty)

	log.Fatal(fasthttp.ListenAndServe(":3000", router.HandleFastHTTP))
}
