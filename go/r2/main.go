package main

import (
	"context"
	"net"
	"net/http"

	"github.com/aofei/r2"
)

func main() {
	r := &r2.Router{}

	r.Handle(http.MethodGet, "/", http.HandlerFunc(func(rw http.ResponseWriter, req *http.Request) {
		rw.Write([]byte(""))
	}))

	r.Handle(http.MethodPost, "/user", http.HandlerFunc(func(rw http.ResponseWriter, req *http.Request) {
		rw.Write([]byte(""))
	}))

	r.Handle(http.MethodGet, "/user/:id", http.HandlerFunc(func(rw http.ResponseWriter, req *http.Request) {
		rw.Write([]byte(r2.PathParam(req, "id")))
	}))

	(&http.Server{
		Addr:    ":3000",
		Handler: r,
		BaseContext: func(_ net.Listener) context.Context {
			return r2.Context()
		},
	}).ListenAndServe()
}
