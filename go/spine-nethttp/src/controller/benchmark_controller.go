package controller

import (
	"github.com/NARUBROWN/spine/pkg/api"
	"github.com/NARUBROWN/spine/pkg/httpx"
	"github.com/NARUBROWN/spine/pkg/input"
)

type BenchmarkController struct{}

func (*BenchmarkController) Constructor() *BenchmarkController {
	return &BenchmarkController{}
}

func (*BenchmarkController) RegisterRoutes(r *api.Registry) {
	r.GET("/", func() httpx.Response[string] {
		return httpx.Response[string]{Body: ""}
	})
	r.GET("/user/:id", func(id input.Path[string]) httpx.Response[string] {
		return httpx.Response[string]{Body: *id}
	})
	r.POST("/user", func() httpx.Response[string] {
		return httpx.Response[string]{Body: ""}
	})
}
