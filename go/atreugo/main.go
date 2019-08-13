package main

import (
	"github.com/savsgio/atreugo/v8"
)

func main() {
	config := &atreugo.Config{
		Host: "0.0.0.0",
		Port: 3000,
	}
	server := atreugo.New(config)

	server.Path("GET", "/", func(ctx *atreugo.RequestCtx) error {
		return ctx.TextResponse("")
	})
	server.Path("GET", "/user/:id", func(ctx *atreugo.RequestCtx) error {
		return ctx.TextResponse(ctx.UserValue("id"))
	})
	server.Path("POST", "/user", func(ctx *atreugo.RequestCtx) error {
		return ctx.TextResponse("")
	})

	err := server.ListenAndServe()
	if err != nil {
		panic(err)
	}
}
