package main

import (
	"log"

	"github.com/gofiber/fiber/v2"
)

const id = "id"

var (
	handlerOK = func(c *fiber.Ctx) error {
		return nil
	}
	handlerID = func(c *fiber.Ctx) error {
		return c.SendString(c.Params(id))
	}
)

func main() {
	app := fiber.New(fiber.Config{
		Prefork:                  false,
		CaseSensitive:            true,
		StrictRouting:            true,
		DisableStartupMessage:    true,
		DisableHeaderNormalizing: true,
	})

	app.Get("/", handlerOK)
	app.Get("/user/:id", handlerID)

	app.Post("/user", handlerOK)

	log.Fatal(app.Listen(":3000"))
}
