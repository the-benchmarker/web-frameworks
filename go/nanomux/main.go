package main

import (
	"net/http"

	"github.com/shohruhadham/nanomux"
)

func handler(w http.ResponseWriter, r *http.Request, args *nanomux.Args) bool {
	w.Write([]byte(""))
	return true
}

func getUser(w http.ResponseWriter, r *http.Request, args *nanomux.Args) bool {
	w.Write([]byte(args.HostPathValues().Get("id")))
	return true
}

func main() {
	var router = nanomux.NewRouter()
	router.SetURLHandlerFor("GET", "/", handler)
	router.SetURLHandlerFor("GET", "/user/{id}", getUser)
	router.SetURLHandlerFor("POST", "/user", handler)

	http.ListenAndServe(":3000", router)
}
