package main

import (
	"fmt"

	"github.com/valyala/fasthttp"
	"github.com/vardius/gorouter/v4"
)

func index(ctx *fasthttp.RequestCtx) {
	fmt.Fprint(ctx, "")
}

func user(ctx *fasthttp.RequestCtx) {
	fmt.Fprint(ctx, "")
}

func userID(ctx *fasthttp.RequestCtx) {
	fmt.Fprint(ctx, ctx.UserValue("id"))
}

func main() {
	router := gorouter.NewFastHTTPRouter()
	
	router.GET("/", index)
	router.GET("/user/{id:[0-9]+}", userID)
	router.POST("/user", user)

	fasthttp.ListenAndServe(":3000", router.HandleFastHTTP)
}
