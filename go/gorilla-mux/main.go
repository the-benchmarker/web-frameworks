package main

import (
	"net/http"

	"github.com/gorilla/mux"
)

func main() {
	r := mux.NewRouter()

	r.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		if r.Method == "GET" {
			w.Write([]byte(""))
		}
	})
	r.HandleFunc("/user/{id}", func(w http.ResponseWriter, r *http.Request) {
		if r.Method == "GET" {
			vars := mux.Vars(r)
			w.Write([]byte(vars["id"]))
		}
	})
	r.HandleFunc("/user", func(w http.ResponseWriter, r *http.Request) {
		if r.Method == "POST" {
			w.Write([]byte(""))
		}
	})

	http.Handle("/", r)
	http.ListenAndServe(":3000", r)
}
