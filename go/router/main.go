package main

import (
	"fmt"
	"log"

	"github.com/fasthttp/router"
	"github.com/valyala/fasthttp"
)

func ShowEmpty(ctx *fasthttp.RequestCtx) {}

func ShowID(ctx *fasthttp.RequestCtx) {
	fmt.Fprintf(ctx, "%s", ctx.UserValue("id"))
}

func main() {
	r := router.New()
	r.GET("/", ShowEmpty)
	r.GET("/user/{id}", ShowID)
	r.POST("/user", ShowEmpty)

	log.Fatal(fasthttp.ListenAndServe(":3000", r.Handler))
}
