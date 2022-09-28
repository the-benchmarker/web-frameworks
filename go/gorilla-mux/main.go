package main

import (
	"net/http"

	"github.com/gorilla/mux"
)

func main() {
	r := mux.NewRouter()

	r.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(""))
	}).Methods("GET")
	r.HandleFunc("/user/{id}", func(w http.ResponseWriter, r *http.Request) {
		vars := mux.Vars(r)
		w.Write([]byte(vars["id"]))
	}).Methods("GET")
	r.HandleFunc("/user", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(""))
	}).Methods("POST")

	http.Handle("/", r)
	http.ListenAndServe(":3000", r)
}
