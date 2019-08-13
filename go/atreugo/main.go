package main

import (
	"fmt"
	"github.com/savsgio/atreugo/v8"
	"strconv"
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
		id := ctx.UserValue("id")
		s, err := fmt.Printf("%s", id)
		if err != nil {
			panic(err)
		}
		return ctx.TextResponse(strconv.Itoa(s))
	})
	server.Path("POST", "/user", func(ctx *atreugo.RequestCtx) error {
		return ctx.TextResponse("")
	})

	err := server.ListenAndServe()
	if err != nil {
		panic(err)
	}
}
