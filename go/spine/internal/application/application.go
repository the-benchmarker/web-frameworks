package application

import (
	"os"

	"github.com/NARUBROWN/spine"
	echoplatform "github.com/NARUBROWN/spine-echo-platform"
	"github.com/NARUBROWN/spine/pkg/boot"
	"github.com/the-benchmarker/web-frameworks/go/spine/src/controller"
	"github.com/the-benchmarker/web-frameworks/go/spine/src/route"
)

// BuildApplication wires constructors and routes and configures runtime options.
func BuildApplication() (spine.App, boot.Options, error) {
	app := spine.New(echoplatform.Option())
	app.Constructor(controller.NewBenchmarkController)
	if err := route.Register(app); err != nil {
		return nil, boot.Options{}, err
	}

	address := ":3000"
	if port := os.Getenv("SPINE_PORT"); port != "" {
		address = ":" + port
	}
	return app, boot.Options{Address: address, HTTP: &boot.HTTPOptions{}}, nil
}
