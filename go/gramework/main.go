package main

import (
	"github.com/gramework/gramework"
	"fmt"
)

func main() {
	app := gramework.New()

	app.GET("/", func(ctx *gramework.Context) error {
		_, err := ctx.WriteString("")
		return err
	})

	app.GET("/user/:id", func(ctx *gramework.Context) error {
                id := ctx.UserValue("id")
		_, err := ctx.WriteString(fmt.Sprintf("%s", id))
		return err
        })

	app.POST("/user", func(ctx *gramework.Context) error {
		_, err := ctx.WriteString("")
		return err;
	})

	app.ListenAndServe("0.0.0.0:3000")
}
