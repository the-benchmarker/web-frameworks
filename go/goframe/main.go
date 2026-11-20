package main

import (
	"net/http"
	"strings"

	"github.com/gogf/gf/v2/frame/g"
)

const (
	routeRoot   = `/`
	routeUser   = `/user`
	routeUserId = `/user/`
)

func main() {
	s := g.Server()
	s.SetHandler(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case routeRoot, routeUser:
			w.WriteHeader(http.StatusOK)

		default:
			w.Header().Set("Content-Type", "text/plain; charset=utf-8")
			if strings.HasPrefix(r.URL.Path, routeUserId) {
				w.Write([]byte(r.URL.Path[len(routeUserId):]))
			} else {
				w.WriteHeader(http.StatusNotFound)
				w.Write([]byte(http.StatusText(http.StatusNotFound)))
			}
		}
	})
	s.SetPort(3000)
	s.Run()
}
