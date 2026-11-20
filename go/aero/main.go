package main

import (
	"github.com/aerogo/aero"
)

func main() {
	app := aero.New()
	app.Config.Ports.HTTP = 3000

	app.Get("/", func(ctx aero.Context) error {
		return ctx.String("")
	})

	app.Get("/user/:id", func(ctx aero.Context) error {
		return ctx.String(ctx.Get("id"))
	})

	app.Post("/user", func(ctx aero.Context) error {
		return ctx.String("")
	})

	app.Run()
}
