package main

import (
	"github.com/savsgio/atreugo/v10"
)

func main() {
	config := atreugo.Config{
		Addr: "0.0.0.0:3000",
	}
	server := atreugo.New(config)

	server.GET("/", func(ctx *atreugo.RequestCtx) error {
		return nil
	})
	server.GET("/user/:id", func(ctx *atreugo.RequestCtx) error {
		id := ctx.UserValue("id").(string)
		return ctx.TextResponse(id)
	})
	server.POST("/user", func(ctx *atreugo.RequestCtx) error {
		return nil
	})

	err := server.ListenAndServe()
	if err != nil {
		panic(err)
	}
}
