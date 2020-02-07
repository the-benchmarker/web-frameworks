package main

import (
	"github.com/gofiber/fiber"
)

func main() {
	app := fiber.New()
	app.Prefork = true
	app.Get("/", func(c *fiber.Ctx) {})
	app.Get("/user/:id", func(c *fiber.Ctx) {
		c.SendString(c.Params("id"))
	})
	app.Post("/user", func(c *fiber.Ctx) {})
	app.Listen(3000)
}
