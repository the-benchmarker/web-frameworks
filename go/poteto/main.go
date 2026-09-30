package main

import (
	"github.com/poteto0/poteto"
)

func main() {
	option := poteto.PotetoOption{
		WithRequestId:   false,
		ListenerNetwork: "tcp",
	}
	p := poteto.NewWithOption(option)

	p.GET("/", func(ctx poteto.Context) error {
		_, err := ctx.GetResponse().Write([]byte(""))
		return err
	})

	p.GET("/user/:id", func(ctx poteto.Context) error {
		id, _ := ctx.PathParam("id")
		_, err := ctx.GetResponse().Write([]byte(id))
		return err
	})

	p.POST("/user", func(ctx poteto.Context) error {
		_, err := ctx.GetResponse().Write([]byte(""))
		return err
	})

	p.Run(":3000")
}
