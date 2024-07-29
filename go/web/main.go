package main

import (
	"git.akyoto.dev/go/web"
)

func main() {
	server := web.NewServer()

	server.Get("/", func(ctx web.Context) error {
		return ctx.String("")
	})

	server.Get("/user/:id", func(ctx web.Context) error {
		return ctx.String(ctx.Request().Param("id"))
	})

	server.Router().Add("POST", "/user", func(ctx web.Context) error {
		return ctx.String("")
	})

	server.Run(":3000")
}
