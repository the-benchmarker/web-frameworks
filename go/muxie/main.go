package main

import (
	"net/http"

	"github.com/kataras/muxie"
)

func main() {
	r := muxie.NewMux()

	r.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		if r.Method == "GET" {
			w.Write([]byte(""))
		}
	})

	r.HandleFunc("/user/:id", func(w http.ResponseWriter, r *http.Request) {
		if r.Method == "GET" {
			w.Write([]byte(muxie.GetParam(w, "id")))
		}
	})

	r.HandleFunc("/user", func(w http.ResponseWriter, r *http.Request) {
		if r.Method == "POST" {
			w.Write([]byte(""))
		}
	})

	http.ListenAndServe(":3000", r)
}
