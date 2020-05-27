package main

import (
	"log"

	"github.com/gofiber/fiber"
)

const empty = func(c *fiber.Ctx) {}
const sendID = func(c *fiber.Ctx) {
	c.SendString(c.Params("id"))
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
	app.Listen(3000)
}
