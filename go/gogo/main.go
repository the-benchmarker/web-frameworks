package main

import (
	"log"
	"runtime"

	gogo "github.com/Snocko-main/gogo"
)

func main() {
	runtime.LockOSThread()
	defer runtime.UnlockOSThread()

	app, err := gogo.NewApp()
	if err != nil {
		log.Fatal(err)
	}
	defer app.Close()

	app.Get("/", gogo.Reply{Status: 200})

	app.Get("/user/:id", func(res *gogo.Response, req *gogo.Request) {
		res.Send(200, "text/plain; charset=utf-8", req.Parameter(0))
	})

	app.Post("/user", func(res *gogo.Response, req *gogo.Request) {
		res.Send(200, "", "")
	})

	if !app.Listen(3000) {
		log.Fatal("failed to listen on :3000")
	}

	app.Run()
}
