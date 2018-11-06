package main

import "github.com/kataras/iris"

func main() {
	app := iris.New()

	app.Get("/", func(ctx iris.Context) {
		ctx.WriteString("")
	})

	app.Get("/user/{id}", func(ctx iris.Context) {
		ctx.WriteString(ctx.Params().Get("id"))
	})

	app.Post("/user", func(ctx iris.Context) {
		ctx.WriteString("")
	})

	app.Run(iris.Addr(":3000"))
}
