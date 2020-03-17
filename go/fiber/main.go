package main

import (
	"strings"

	"github.com/gofiber/fiber"
)

const pathUser = "/user/"

func main() {
	app := fiber.New(&fiber.Settings{
		Prefork:       true,
		CaseSensitive: true,
		StrictRouting: true,
	})
	app.Use(func(c *fiber.Ctx) {
		if strings.HasPrefix(c.Path(), pathUser) {
			c.SendString(c.Path()[6:])
		}
	})
	app.Listen(3000)
}
