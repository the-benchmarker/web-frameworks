package main

import (
	"github.com/aurora-go/aurora/aurora"
	"github.com/aurora-go/aurora/aurora/req"
)

func main() {
	//获取 aurora 路由实例
	a := aurora.New()

	a.GET("/", func(request aurora.Request) interface{} {
		return ""
	})
	a.GET("/user/{id}", func(request aurora.Request) interface{} {
		args := request[req.Args].(map[string]interface{})
		return args["id"]
	})
	a.POST("/user", func(request aurora.Request) interface{} {
		return ""
	})

	// 启动服务器 默认端口8080，更改端口号 a.Guide(”8081“) 即可
	a.Guide("3000")
}