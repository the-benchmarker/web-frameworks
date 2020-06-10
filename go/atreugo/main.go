package main

import (
	"log"

	"github.com/savsgio/atreugo/v11"
)

func showEmpty(ctx *atreugo.RequestCtx) error {
	return nil
}

func showID(ctx *atreugo.RequestCtx) error {
	id := ctx.UserValue("id").(string)

	return ctx.TextResponse(id)
}

func main() {
	server := atreugo.New(atreugo.Config{
		Addr: ":3000",
	})

	server.GET("/", showEmpty)
	server.GET("/user/{id}", showID)
	server.POST("/user", showEmpty)

	log.Fatal(server.ListenAndServe())
}
