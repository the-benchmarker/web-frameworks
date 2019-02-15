package main

import (
	"net/http"

	"github.com/labstack/echo/v4"
)

func main() {
	e := echo.New()

	e.GET("/", func(c echo.Context) error {
		return c.String(http.StatusOK, "")
	})

	e.GET("/user/:id", func(c echo.Context) error {
		return c.String(http.StatusOK, c.Param("id"))
	})

	e.POST("/user", func(c echo.Context) error {
		return c.String(http.StatusOK, "")
	})

	e.Start(":3000")
}
