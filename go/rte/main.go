package main

import (
	"github.com/jwilner/rte"
	"log"
	"net/http"
	"os"
)

func main() {
	log.Fatal(http.ListenAndServe(os.Getenv("SERVER_ADDRESS"), rte.Must(rte.Routes(
		"GET /", func(w http.ResponseWriter, r *http.Request) {},
		"/user", rte.Routes(
			"POST", func(w http.ResponseWriter, r *http.Request) {},
			"GET /:id", func(w http.ResponseWriter, r *http.Request, id string) {
				_, _ = w.Write([]byte(id))
			},
		),
	))))
}
