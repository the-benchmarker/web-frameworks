package main

import (
	"net/http"

	"github.com/uptrace/bunrouter"
)

func main() {
	r := bunrouter.New()

	r.GET("/", func(w http.ResponseWriter, req bunrouter.Request) error {
		w.Write([]byte(""))
		return nil
	})

	r.GET("/user/:name", func(w http.ResponseWriter, req bunrouter.Request) error {
		name := req.Param("name")
		w.Write([]byte(name))
		return nil
	})

	r.POST("/user", func(w http.ResponseWriter, req bunrouter.Request) error {
		w.Write([]byte(""))
		return nil
	})

	http.ListenAndServe(":3000", r)
}
