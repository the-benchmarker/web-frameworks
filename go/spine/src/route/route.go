package route

import (
	"github.com/NARUBROWN/spine"
	controller "github.com/the-benchmarker/web-frameworks/go/spine/src/controller"
)

func Register(app spine.App) error {
	app.Route("POST", "/user", (*controller.BenchmarkController).CreateUser)
	app.Route("GET", "/user/:id", (*controller.BenchmarkController).User)
	app.Route("GET", "/", (*controller.BenchmarkController).Index)
	return nil
}
