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
	app.Get("/user/:id", func(c *fiber.Ctx) {
		c.SendString(c.Params("id"))
	})
	app.Use(func(c *fiber.Ctx) {
		return
	})
	app.Listen(3000)
}
