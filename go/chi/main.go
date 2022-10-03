package main

import (
	"net/http"

	"github.com/go-chi/chi/v5"
)

func main() {
	r := chi.NewRouter()

	r.Get("/", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(""))
	})

	r.Get("/user/{id}", func(w http.ResponseWriter, r *http.Request) {
		id := chi.URLParam(r, "id")
		w.Write([]byte(id))
	})

	r.Post("/user", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(""))
	})

	http.ListenAndServe(":3000", r)
}
