package main

import (
	"log"

	"github.com/gofiber/fiber"
	"github.com/gofiber/utils"
)

const id = "id"

var (
	handlerOK = func(c *fiber.Ctx) {}
	handlerID = func(c *fiber.Ctx) {
		c.SendBytes(utils.GetBytes(c.Params(id)))
	}
)

func main() {
	app := fiber.New(&fiber.Settings{
		Prefork:                   true,
		CaseSensitive:             true,
		StrictRouting:             true,
		DisableDefaultDate:        true,
		DisableStartupMessage:     true,
		DisableHeaderNormalizing:  true,
		DisableDefaultContentType: true,
	})

	app.Get("/", handlerOK)
	app.Get("/user/:id", handlerID)

	app.Post("/user", handlerOK)

	log.Fatal(app.Listen(3000))
}
