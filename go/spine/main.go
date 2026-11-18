package main

import (
	"io"
	"log"
	"os"

	"github.com/the-benchmarker/web-frameworks/go/spine/internal/application"
)

func main() {
	log.SetOutput(io.Discard)

	app, options := application.BuildApplication()
	if err := app.Validate(options); err != nil {
		os.Exit(1)
	}
	if err := app.Run(options); err != nil {
		os.Exit(1)
	}
}
