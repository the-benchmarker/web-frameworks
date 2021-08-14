package main

import (
	"fmt"
	"github.com/cnotch/apirouter"
	"net/http"
)

func main() {
	r := apirouter.New(
		apirouter.API("GET", "/", func(w http.ResponseWriter, r *http.Request, ps apirouter.Params) {
			fmt.Fprint(w, "")
		}),
		apirouter.API("GET", "/user/:id", func(w http.ResponseWriter, r *http.Request, ps apirouter.Params) {
			id := ps.ByName("id")
			fmt.Fprint(w, id)
		}),
		apirouter.API("POST", "/user", func(w http.ResponseWriter, r *http.Request, ps apirouter.Params) {
			fmt.Fprint(w, "")
		}),
	)

	http.ListenAndServe(":3000", r)
}
