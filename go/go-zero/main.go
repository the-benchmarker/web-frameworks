package main

import (
	"net/http"

	"github.com/zeromicro/go-zero/core/logx"
	"github.com/zeromicro/go-zero/core/service"
	"github.com/zeromicro/go-zero/rest"
	"github.com/zeromicro/go-zero/rest/httpx"
)

func main() {

	engine := rest.MustNewServer(rest.RestConf{
		ServiceConf: service.ServiceConf{
			Log: logx.LogConf{
				Mode: "console", // default=console,options=[console,file,volume]
			},
		},
		Port:     3000,
		Timeout:  3000,    // milliseconds
		MaxConns: 1000000, // default=10000
	})
	defer engine.Stop()

	engine.AddRoutes([]rest.Route{
		{
			Method: http.MethodGet,
			Path:   "/",
			Handler: func(w http.ResponseWriter, r *http.Request) {
				w.Write([]byte(""))
			},
		},

		{
			Method: http.MethodGet,
			Path:   "/user/:name",
			Handler: func(w http.ResponseWriter, r *http.Request) {
				var v struct {
					Name string `path:"name"`
				}
				httpx.ParsePath(r, &v)
				w.Write([]byte(v.Name))
			},
		},

		{
			Method: http.MethodPost,
			Path:   "/user",
			Handler: func(w http.ResponseWriter, r *http.Request) {
				w.Write([]byte(""))
			},
		},
	})

	engine.Start()
}
