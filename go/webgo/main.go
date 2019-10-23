package main

import (
	"fmt"
	"net/http"
	"time"

	"github.com/bnkamalesh/webgo/v3"
)

func empty(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, "")
	// webgo.Send(w, "text", "text/plain", http.StatusOK)
}

func userID(w http.ResponseWriter, r *http.Request) {
	wctx := webgo.Context(r)
	fmt.Fprint(w, wctx.Params["id"])
	// webgo.Send(w, "text/plain", wctx.Params["id"], http.StatusOK)
}

func getRoutes() []*webgo.Route {
	return []*webgo.Route{
		&webgo.Route{
			Method:        http.MethodGet,            // request type
			Pattern:       "/",                       // Pattern for the route
			Handlers:      []http.HandlerFunc{empty}, // route handler
			TrailingSlash: true,
		},
		&webgo.Route{
			Method:        http.MethodGet,             // request type
			Pattern:       "/user/:id",                // Pattern for the route
			Handlers:      []http.HandlerFunc{userID}, // route handler
			TrailingSlash: true,
		},
		&webgo.Route{
			Name:          "api",                     // A label for the API/URI, this is not used anywhere.
			Method:        http.MethodGet,            // request type
			Pattern:       "/user",                   // Pattern for the route
			Handlers:      []http.HandlerFunc{empty}, // route handler
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
	// router.Use(middleware.AccessLog)
	router.Start()
}
