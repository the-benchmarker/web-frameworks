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
		return
	})
	app.Get("/user/:id", func(c *fiber.Ctx) {
		c.SendString(c.Params("id"))
	})
	// app.Get("/user/1", func(c *fiber.Ctx) {
	// 	c.JSON("")
	// })
	app.Post("/user", func(c *fiber.Ctx) {
		return
	})
	// app.Put("/user/1", func(c *fiber.Ctx) {
	// 	c.JSON("")
	// })
	// app.Patch("/user/1", func(c *fiber.Ctx) {
	// 	c.JSON("")
	// })
	// app.Delete("/user/1", func(c *fiber.Ctx) {
	// 	c.JSON("")
	// })

	app.Listen(3000)
}
