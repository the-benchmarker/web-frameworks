package main

import (
	"net/http"

	"gopkg.in/macaron.v1"
)

func main() {
	m := macaron.Classic()
	m.Get("/", func() string {
		return ""
	})

	m.Get("/user/:name", func(c *macaron.Context) string {
		return c.Params("name")
	})

	m.Post("/user", func() string {
		return ""
	})

	http.ListenAndServe("0.0.0.0:3000", m)
}
