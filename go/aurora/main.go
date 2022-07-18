package main

import (
	"gitee.com/aurora-engine/aurora"
)

func main() {
	a := aurora.NewAurora()
	a.Get("/", func() string {
		return ""
	})
	a.Get("/user/{id}", func(id string) string {
		return id
	})
	a.Post("/user", func() string {
		return ""
	})
	err := aurora.Run(a)
	if err != nil {
		a.Error(err.Error())
		return
	}
}
