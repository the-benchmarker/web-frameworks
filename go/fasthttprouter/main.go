package main

import (
	"fmt"
	"log"

	"github.com/buaazp/fasthttprouter"
	"github.com/valyala/fasthttp"
)

func showEmpty(ctx *fasthttp.RequestCtx) {}

func showID(ctx *fasthttp.RequestCtx) {
	fmt.Fprintf(ctx, "%s", ctx.UserValue("id"))
}

func main() {
	router := fasthttprouter.New()
	router.GET("/", showEmpty)
	router.GET("/user/:id", showID)
	router.POST("/user", showEmpty)

	log.Fatal(fasthttp.ListenAndServe(":3000", router.Handler))
}
