package application

import (
	"os"

	"github.com/NARUBROWN/spine"
	echoplatform "github.com/NARUBROWN/spine-echo-platform"
	"github.com/NARUBROWN/spine/pkg/boot"
	"github.com/NARUBROWN/spine/pkg/constructor"
	"github.com/the-benchmarker/web-frameworks/go/spine/src/controller"
)

// BuildApplication wires constructors and routes and configures runtime options.
func BuildApplication() (spine.App, boot.Options) {
	app := spine.New(echoplatform.Option())
	app.Constructor(constructor.Controller(&controller.BenchmarkController{}))

	address := ":3000"
	if port := os.Getenv("SPINE_PORT"); port != "" {
		address = ":" + port
	}
	return app, boot.Options{Address: address, HTTP: &boot.HTTPOptions{}}
}
