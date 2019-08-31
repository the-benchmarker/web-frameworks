package main

import (
	"net/http"

	"github.com/vardius/gorouter/v4"
	"github.com/vardius/gorouter/v4/context"
)

func index(w http.ResponseWriter, r *http.Request) {
	w.Write([]byte(""))
}

func user(w http.ResponseWriter, r *http.Request) {
	w.Write([]byte(""))
}

func userID(w http.ResponseWriter, r *http.Request) {
	params, _ := context.Parameters(r.Context())
	id := params.Value("id")
	w.Write([]byte(id))
}

func main() {
	router := gorouter.New()

	router.GET("/", http.HandlerFunc(index))
	router.GET("/user/{id:[0-9]+}", http.HandlerFunc(userID))
	router.POST("/user", http.HandlerFunc(user))

	http.ListenAndServe(":3000", router)
}
