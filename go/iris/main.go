package main

import (
	"github.com/kataras/iris"
	"github.com/kataras/iris/context"
)

func main() {
	app := iris.New()

	app.Get("/", func(ctx context.Context) {
		ctx.WriteString("")
	})

	app.Get("/user/{id}", func(ctx context.Context) {
		ctx.WriteString(ctx.Params().Get("id"))
	})

	app.Post("/user", func(ctx context.Context) {
		ctx.WriteString("")
	})

	app.Run(iris.Addr("localhost:3000"))
}
