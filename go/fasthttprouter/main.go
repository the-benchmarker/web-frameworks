package main

import (
	"fmt"
	"log"

	"github.com/buaazp/fasthttprouter"
	"github.com/valyala/fasthttp"
)

func ShowEmpty(ctx *fasthttp.RequestCtx) {}

func ShowID(ctx *fasthttp.RequestCtx) {
	fmt.Fprintf(ctx, "%s", ctx.UserValue("id"))
}

func main() {
	router := fasthttprouter.New()
	router.GET("/", ShowEmpty)
	router.GET("/user/:id", ShowID)
	router.POST("/user", ShowEmpty)

	log.Fatal(fasthttp.ListenAndServe(":3000", router.Handler))
}
