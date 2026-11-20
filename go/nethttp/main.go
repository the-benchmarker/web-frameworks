package main

import (
	"net/http"
)

func main() {
	mux := http.NewServeMux()

	mux.HandleFunc("GET /", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(""))
	})

	mux.HandleFunc("GET /user/{name}", func(w http.ResponseWriter, r *http.Request) {
		name := r.PathValue("name")
		w.Write([]byte(name))
	})

	mux.HandleFunc("POST /user", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(""))
	})

	http.ListenAndServe(":3000", mux)
}
