package controller

import (
	"github.com/NARUBROWN/spine/pkg/httpx"
	"github.com/NARUBROWN/spine/pkg/path"
)

type BenchmarkController struct{}

func NewBenchmarkController() *BenchmarkController { return &BenchmarkController{} }

func (c *BenchmarkController) Index() httpx.Response[string] {
	return httpx.Response[string]{Body: ""}
}

func (c *BenchmarkController) User(id path.String) httpx.Response[string] {
	return httpx.Response[string]{Body: id.Value}
}

func (c *BenchmarkController) CreateUser() httpx.Response[string] {
	return httpx.Response[string]{Body: ""}
}
