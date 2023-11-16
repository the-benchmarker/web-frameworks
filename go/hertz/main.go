package main

import (
	"context"
	"github.com/cloudwego/hertz/pkg/app"
	"github.com/cloudwego/hertz/pkg/app/server"
)

func main() {

	r := server.New(
		server.WithHostPorts(":3000"),
		server.WithDisableHeaderNamesNormalizing(true),
		server.WithDisablePrintRoute(true),
		server.WithDisableDefaultDate(true),
		server.WithDisableDefaultContentType(true),
	)

	r.GET("/", func(_ context.Context, ctx *app.RequestContext) {})
	r.GET("/user/:name", func(_ context.Context, ctx *app.RequestContext) {
		ctx.Response.SetBodyString(ctx.Param("name"))
	})
	r.POST("/user", func(_ context.Context, ctx *app.RequestContext) {})
	r.Spin()
}
