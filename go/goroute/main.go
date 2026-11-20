package main

import (
	"log"
	"net/http"

	"github.com/goroute/route"
)

func main() {
	mux := route.NewServeMux()

	mux.GET("/", func(c route.Context) error {
		return c.String(http.StatusOK, "")
	})

	mux.GET("/user/:id", func(c route.Context) error {
		return c.String(http.StatusOK, c.Param("id"))
	})

	mux.POST("/user", func(c route.Context) error {
		return c.String(http.StatusOK, "")
	})

	log.Fatal(http.ListenAndServe(":3000", mux))
}
