package application

import (
	"os"

	"github.com/NARUBROWN/spine"
	"github.com/NARUBROWN/spine/pkg/boot"
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

// BuildApplication은 서버 실행, 계약 검사와 오프라인 검증이 함께 사용하는 composition root입니다.
func BuildApplication() (spine.App, boot.Options, error) {
	app := spine.New()
	app.Constructor(NewBenchmarkController)
	app.Route("GET", "/", (*BenchmarkController).Index)
	app.Route("GET", "/user/:id", (*BenchmarkController).User)
	app.Route("POST", "/user", (*BenchmarkController).CreateUser)

	address := ":3000"
	if port := os.Getenv("SPINE_PORT"); port != "" {
		address = ":" + port
	}
	return app, boot.Options{Address: address, HTTP: &boot.HTTPOptions{}}, nil
}
