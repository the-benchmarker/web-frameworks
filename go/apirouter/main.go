package main

import (
	"github.com/cnotch/apirouter"
	"net/http"
	"io"
)


func main() {
	r := apirouter.New(
		apirouter.API("GET", "/", func(w http.ResponseWriter, r *http.Request, ps apirouter.Params) {
			io.WriteString(w, "")

		}),
		apirouter.API("GET", "/user/:id", func(w http.ResponseWriter, r *http.Request, ps apirouter.Params) {
			id := ps.ByName("id")
			io.WriteString(w, id)
		}),
		apirouter.API("POST", "/user", func(w http.ResponseWriter, r *http.Request, ps apirouter.Params) {
			io.WriteString(w, "")

		}),
	)

	http.ListenAndServe(":3000", r)
}
