package main

import (
	"github.com/gofiber/fiber"
)

func main() {
	app := fiber.New(&fiber.Settings{
		Prefork:       true,
		CaseSensitive: true,
		StrictRouting: true,
	})
	app.Get("/", func(c *fiber.Ctx) {
		c.SendString("")
	})
	app.Get("/user/:id", func(c *fiber.Ctx) {
		c.SendString(c.Params("id"))
	})
	app.Listen(3000)
}
