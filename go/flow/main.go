package main

import (
	"net/http"

	"github.com/alexedwards/flow"
)

func main() {
	mux := flow.New()

	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(""))
	}, http.MethodGet)

	mux.HandleFunc("/user/:id", func(w http.ResponseWriter, r *http.Request) {
		id := r.PathValue("id")
		w.Write([]byte(id))
	}, http.MethodGet)

	mux.HandleFunc("/user", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(""))
	}, http.MethodPost)

	http.ListenAndServe(":3000", mux)
}
