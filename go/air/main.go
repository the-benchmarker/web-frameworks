package main

import "github.com/aofei/air"

func main() {
	a := air.Default
	a.Address = ":3000"

	a.GET("/", func(req *air.Request, res *air.Response) error {
		return res.WriteString("")
	})

	a.GET("/user/:id", func(req *air.Request, res *air.Response) error {
		return res.WriteString(req.Param("id").Value().String())
	})

	a.POST("/user", func(req *air.Request, res *air.Response) error {
		return res.WriteString("")
	})

	a.Serve()
}
