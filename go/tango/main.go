package main

import (
	"gitea.com/lunny/tango"
)

type Params struct {
	tango.Params
}

func (req *Params) Get() string {
	return req.Params.Get(":id")
}

type Empty struct{}

func (Empty) Get() string {
	return ""
}

func (Empty) Post() string {
	return ""
}

func main() {
	t := tango.New(
		tango.Logging(),
		tango.Recovery(false),
		tango.Return(),
		tango.Param(),
	)
	t.Get("/", new(Empty))
	t.Get("/user/:id", new(Params))
	t.Post("/user", new(Empty))
	t.Run()
}
