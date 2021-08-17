package main

import (
	beego "github.com/beego/beego/v2/server/web"
	"github.com/beego/beego/v2/server/web/context"
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

	beego.Run(":8080")
}
