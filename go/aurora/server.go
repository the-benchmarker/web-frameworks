package main

import (
	"gitee.com/aurora-engine/aurora"
)

type Server struct {
	*aurora.Engine
}

func (server *Server) Server() {

}

func (server *Server) Router() {
	server.Get("/", func() string {
		return ""
	})
	server.Get("/user/{id}", func(id string) string {
		return id
	})
	server.Post("/user", func() string {
		return ""
	})
}
