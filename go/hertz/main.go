package main

import (
	"context"
	"github.com/cloudwego/hertz/pkg/app"
	"github.com/cloudwego/hertz/pkg/app/server"
)

func main() {

	r := server.New(server.WithHostPorts(":3000"))

	r.GET("/", func(_ context.Context, ctx *app.RequestContext) {
		ctx.Write([]byte(""))
	})

	r.GET("/user/:name", func(_ context.Context, ctx *app.RequestContext) {
		name := ctx.Params.ByName("name")
		ctx.Write([]byte(name))
	})

	r.POST("/user", func(_ context.Context, ctx *app.RequestContext) {
		ctx.Write([]byte(""))
	})

	r.Spin()
}
