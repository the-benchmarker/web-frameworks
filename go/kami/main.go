package main

import (
	"context"
	"fmt"
	"net/http"

	"github.com/guregu/kami"
)

func Index(ctx context.Context, w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "")
}

func GetUser(ctx context.Context, w http.ResponseWriter, r *http.Request) {
	id := kami.Param(ctx, "id")
	fmt.Fprintf(w, id)

}

func CreateUser(ctx context.Context, w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "")
}

func main() {
	kami.Get("/", Index)
	kami.Get("/user/:id", GetUser)
	kami.Post("/user", CreateUser)
	kami.Serve()
}
