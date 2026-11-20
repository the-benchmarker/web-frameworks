package main

import (
	"io"

	"clevergo.tech/clevergo"
)

func dummy(*clevergo.Context) error {
	return nil
}

func user(c *clevergo.Context) error {
	io.WriteString(c.Response, c.Params.String("name"))
	return nil
}

func main() {
	app := clevergo.Pure()
	app.Get("/", dummy)
	app.Get("/user/:name", user)
	app.Post("/user", dummy)
	app.Run(":3000")
}
