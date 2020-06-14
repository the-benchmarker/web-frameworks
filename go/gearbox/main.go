package main

import (
	"log"

	"github.com/gogearbox/gearbox"
)

const id = "id"

var empty = func(c *gearbox.Context) {}
var sendID = func(c *gearbox.Context) {
	c.RequestCtx.Response.SetBodyString(c.Params.GetString(id).(string))
}

func main() {
	app := gearbox.New()
	app.Get("/", empty)
	app.Get("/user/:id", sendID)
	app.Post("/user", empty)
	log.Fatal(app.Start(":3000"))
}
