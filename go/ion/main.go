package main

import (
	"github.com/get-ion/ion"
	"github.com/get-ion/ion/context"
)

func main() {
	app := ion.New()

	app.Get("/", func(ctx context.Context) {
		ctx.Write([]byte(""))
	})
	app.Get("/user/{id}", func(ctx context.Context) {
		id := ctx.Params().Get("id")
		w.Write([]byte(id))
	})
	app.Post("/user", func(ctx context.Context) {
		w.Write([]byte(""))
	})

	app.Run(":3000", r)
}
