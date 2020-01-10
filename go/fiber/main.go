package main

import (
	"github.com/fenny/fiber"
)

func main() {
	app := fiber.New()
	app.Get("/", func(c *fiber.Ctx) {
		c.Send("")
	})
	app.Get("/user/:id", func(c *fiber.Ctx) {
		c.Send(c.Params("id"))
	})
	app.Post("/", func(c *fiber.Ctx) {
		c.Send("")
	})
	app.Listen(3000)
}
