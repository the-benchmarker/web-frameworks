package main

import (
	"github.com/fenny/fiber"
)

func main() {
	app := fiber.New()
	app.Get("/", func(c *fiber.Ctx) {

	})
	app.Get("/user/:id", func(c *fiber.Ctx) {
		c.SendString(c.Params("id"))
	})
	app.Post("/user", func(c *fiber.Ctx) {

	})
	app.Listen(3000)
}
