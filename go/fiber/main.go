package main

import (
	"log"

	"github.com/gofiber/fiber"
)

const id = "id"

var empty = func(c *fiber.Ctx) {}
var sendID = func(c *fiber.Ctx) {
	c.SendString(c.Params(id))
}

func main() {
	app := fiber.New(&fiber.Settings{
		Prefork:       true,
		CaseSensitive: true,
		StrictRouting: true,
	})
	app.Get("/", empty)
	app.Get("/user/:id", sendID)
	app.Post("/user", empty)
	log.Fatal(app.Listen(3000))
}
