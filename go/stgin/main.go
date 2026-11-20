package main

import (
	"github.com/AminMal/stgin"
)

func root(stgin.RequestContext) stgin.Status {
	return stgin.Ok(stgin.Empty())
}

func getUser(request stgin.RequestContext) stgin.Status {
	id := request.PathParams.MustGet("name")
	return stgin.Ok(stgin.Text(id))
}

func newUser(request stgin.RequestContext) stgin.Status {
	return stgin.Ok(stgin.Empty())
}

func main() {
	server := stgin.NewServer(":3000")
	server.AddRoutes(
		stgin.GET("/", root),
		stgin.GET("/user/$name", getUser),
		stgin.POST("/user", newUser),
	)
	server.Start()
}
