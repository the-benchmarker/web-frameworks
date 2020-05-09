package main

import (
	"github.com/labstack/echo/v4"
)

func main() {
	e := echo.New()

	e.GET("/", func(c echo.Context) error {
		_, err := c.Response().Write([]byte(""))
		return err
	})

	e.GET("/user/:id", func(c echo.Context) error {
		id := c.Param("id")
		_, err := c.Response().Write([]byte(id))
		return err
	})

	e.POST("/user", func(c echo.Context) error {
		_, err := c.Response().Write([]byte(""))
		return err
	})

	e.Start(":3000")
}
