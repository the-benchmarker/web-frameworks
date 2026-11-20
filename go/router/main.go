package main

import (
	"fmt"
	"log"

	"github.com/fasthttp/router"
	"github.com/valyala/fasthttp"
)

func showEmpty(ctx *fasthttp.RequestCtx) {}

func showID(ctx *fasthttp.RequestCtx) {
	fmt.Fprintf(ctx, "%s", ctx.UserValue("id"))
}

func main() {
	r := router.New()
	r.GET("/", showEmpty)
	r.GET("/user/{id}", showID)
	r.POST("/user", showEmpty)

	log.Fatal(fasthttp.ListenAndServe(":3000", r.Handler))
}
