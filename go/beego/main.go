package main

import (
	"github.com/astaxie/beego"
	"github.com/astaxie/beego/context"
)

func main() {
	beego.Get("/", func(ctx *context.Context) {
		ctx.WriteString("")
	})

	beego.Get("/user/:id", func(ctx *context.Context) {
		ctx.WriteString(ctx.Input.Param(":id"))
	})

	beego.Post("/user", func(ctx *context.Context) {
		ctx.WriteString("")
	})

	beego.Run(":3000")
}
