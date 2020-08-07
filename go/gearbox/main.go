package main

import (
	"log"

	"github.com/gogearbox/gearbox"
)

var empty = func(c gearbox.Context) {}
var sendID = func(c gearbox.Context) {
	c.SendString(c.Param("id"))
}

func main() {
	app := gearbox.New(&gearbox.Settings{
		Prefork:               false,
		DisableStartupMessage: true,
	})
	app.Get("/", empty)
	app.Get("/user/:id", sendID)
	app.Post("/user", empty)
	log.Fatal(app.Start(":3000"))
}
