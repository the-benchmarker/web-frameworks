package main

import (
	"fmt"
	"net/http"
	"os"

	"goyave.dev/goyave/v5"
	"goyave.dev/goyave/v5/util/errors"
)

func empty(r *goyave.Response, _ *goyave.Request) {
	r.Status(http.StatusOK)
}

func registerRoutes(_ *goyave.Server, router *goyave.Router) {
	router.Get("/", empty)
	router.Get("/user/{id}", func(r *goyave.Response, req *goyave.Request) {
		r.String(http.StatusOK, req.RouteParams["id"])
	})
	router.Post("/user", empty)
}

func main() {
	server, err := goyave.New(goyave.Options{})
	if err != nil {
		fmt.Fprintln(os.Stderr, err.(*errors.Error).String())
		os.Exit(1)
	}

	server.RegisterSignalHook()

	server.RegisterRoutes(registerRoutes)

	if err := server.Start(); err != nil {
		server.Logger.Error(err)
		os.Exit(2)
	}
}
