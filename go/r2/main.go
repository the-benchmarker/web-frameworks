package main

import (
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

	http.ListenAndServe(":3000", r)
}
