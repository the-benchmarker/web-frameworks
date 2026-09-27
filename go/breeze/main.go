package main

import (
	"runtime"

	"github.com/nelthaarion/breeze"
)

func main() {
	router := breeze.NewRouter()

	router.Handle(breeze.GET, "/", func(ctx *breeze.Context) {
		ctx.Status(200)
		ctx.WriteString("")
	})

	router.Handle(breeze.GET, "/user/", func(ctx *breeze.Context) {
		ctx.Status(200)
		ctx.WriteString("")
	})
	router.Handle(breeze.GET, "/user/:id", func(ctx *breeze.Context) {
		ctx.Status(200)
		ctx.WriteString(ctx.GetParam("id"))
	})
	router.Handle(breeze.POST, "/user/", func(ctx *breeze.Context) {
		ctx.Status(200)
		ctx.WriteString("")
	})
	router.Handle(breeze.POST, "/user/:id", func(ctx *breeze.Context) {
		ctx.Status(200)
		ctx.WriteString(ctx.GetParam("id"))
	})
	app := breeze.New(router, breeze.NewWorkerPool(runtime.NumCPU()))
	app.Run(3000, true)
}
