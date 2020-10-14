package main

import (
	"net/http"
	"os"

	"github.com/System-Glitch/goyave/v3"
)

func empty(r *goyave.Response, req *goyave.Request) {
	r.Status(http.StatusOK)
}

func registerRoutes(router *goyave.Router) {
	router.Get("/", empty)
	router.Get("/user/{id}", func(r *goyave.Response, req *goyave.Request) {
		r.String(http.StatusOK, req.Params["id"])
	})
	router.Post("/user", empty)
}

func main() {
	if err := goyave.Start(registerRoutes); err != nil {
		os.Exit(err.(*goyave.Error).ExitCode)
	}
}
