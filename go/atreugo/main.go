package main

import (
	"github.com/savsgio/atreugo/v9"
)

func main() {
	config := &atreugo.Config{
		Addr: "0.0.0.0:3000",
	}
	server := atreugo.New(config)

	server.Path("GET", "/", func(ctx *atreugo.RequestCtx) error {
		return ctx.TextResponse("")
	})
	server.Path("GET", "/user/:id", func(ctx *atreugo.RequestCtx) error {
		id := ctx.UserValue("id").(string)
		return ctx.TextResponse(id)
	})
	server.Path("POST", "/user", func(ctx *atreugo.RequestCtx) error {
		return ctx.TextResponse("")
	})

	err := server.ListenAndServe()
	if err != nil {
		panic(err)
	}
}
