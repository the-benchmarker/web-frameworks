package actions

import (
	"net/http"

	"github.com/gobuffalo/buffalo"
)

func HomeHandler(c buffalo.Context) error {
	return c.Render(http.StatusOK, r.String(""))
}

func UserNameEchoHandler(c buffalo.Context) error {
	return c.Render(http.StatusOK, r.String((c.Param("user"))))
}

func UserNameHandler(c buffalo.Context) error {
	return c.Render(http.StatusOK, r.String(""))
}
