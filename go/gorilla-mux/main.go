package main

import (
	"net/http"
	"github.com/gorilla/mux"
)

func IndexHandler(w http.ResponseWriter, r *http.Request){
	if r.Method == "GET" {
		w.Write([]byte(""))
	}
}

func UserHandler(w http.ResponseWriter, r *http.Request){
	if r.Method == "GET" {
		vars := mux.Vars(r)
		w.Write([]byte(vars["id"]))
	}
}

func RegisterUserHandler(w http.ResponseWriter, r *http.Request){
	if r.Method == "POST" {
		w.Write([]byte(""))
	}
}

func main(){
	r := mux.NewRouter()

	r.HandleFunc("/", IndexHandler)
	r.HandleFunc("/user/{id}", UserHandler)
	r.HandleFunc("/user", RegisterUserHandler)

	http.Handle("/", r)
	http.ListenAndServe(":3000", r)
}
