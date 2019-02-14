package main

import (
	"github.com/gogf/gf/g"
	"github.com/gogf/gf/g/net/ghttp"
)

func main() {
	s := g.Server()
	s.BindHandler("/", func(r *ghttp.Request) {

	})
	s.BindHandler("/user", func(r *ghttp.Request) {

	})
	s.BindHandler("/user/:id", func(r *ghttp.Request) {
		r.Response.Header().Set("Content-Type", "text/plain; charset=utf-8")
		r.Response.Write(r.Get("id"))
	})
	s.SetFileServerEnabled(false)
	s.SetPort(3000)
	s.Run()
}
