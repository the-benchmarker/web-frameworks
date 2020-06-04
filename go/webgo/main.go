package main

import (
	"fmt"
	"net/http"
	"time"

	"github.com/bnkamalesh/webgo/v4"
)

func empty(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, "")
}

func userID(w http.ResponseWriter, r *http.Request) {
	wctx := webgo.Context(r)
	fmt.Fprint(w, wctx.Params()["id"])
}

func getRoutes() []*webgo.Route {
	return []*webgo.Route{
		&webgo.Route{
			Name:          "root",
			Method:        http.MethodGet,
			Pattern:       "/",
			Handlers:      []http.HandlerFunc{empty},
			TrailingSlash: true,
		},
		&webgo.Route{
			Name:          "user-with-URI-params",
			Method:        http.MethodGet,
			Pattern:       "/user/:id",
			Handlers:      []http.HandlerFunc{userID},
			TrailingSlash: true,
		},
		&webgo.Route{
			Name:          "user-without-params",
			Method:        http.MethodPost,
			Pattern:       "/user",
			Handlers:      []http.HandlerFunc{empty},
			TrailingSlash: true,
		},
	}
}

func main() {
	cfg := &webgo.Config{
		Host:         "",
		Port:         "3000",
		ReadTimeout:  10 * time.Second,
		WriteTimeout: 10 * time.Second,
	}

	router := webgo.NewRouter(cfg, getRoutes())
	router.Start()
}
