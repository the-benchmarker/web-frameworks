package main

import (
	"github.com/flamego/flamego"
)

func main() {
	f := flamego.New()
	f.Get("/", func() string { return "" })
	f.Get("/user/{id}", func(c flamego.Context) string { return c.Param("id") })
	f.Post("/user", func() string { return "" })
	f.Run(3000)
}
