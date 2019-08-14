package main

import (
	"github.com/gramework/gramework"
)

func main() {
	app := gramework.New()

	app.GET("/", "")

	app.GET("/user/:id", func(ctx *gramework.Context) {
		ctx.SetBodyString(ctx.RouteArg("id"))
	})

	app.POST("/user", "")

	app.ListenAndServe("0.0.0.0:3000")
}
