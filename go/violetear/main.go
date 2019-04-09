package main

import (
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/nbari/violetear"
)

func showId(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, violetear.GetParam("id", r))
}

func showEmpty(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "")
}

func main() {
	router := violetear.New()

	router.AddRegex(":id", `[0-9]+`)

	router.HandleFunc("/", showEmpty, "GET")
	router.HandleFunc("/user/:id", showId, "GET")
	router.HandleFunc("/user", showEmpty, "POST")

	srv := &http.Server{
		Addr:           ":3000",
		Handler:        router,
		ReadTimeout:    5 * time.Second,
		WriteTimeout:   7 * time.Second,
		MaxHeaderBytes: 1 << 20,
	}
	log.Fatal(srv.ListenAndServe())
}
