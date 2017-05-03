package main

import(
	"fmt"
	"gopkg.in/kataras/iris.v6"
	"gopkg.in/kataras/iris.v6/adaptors/httprouter"
)

func index(context *iris.Context) {
	context.Writef("")
}

func user(context *iris.Context) {
	userId, _ := context.ParamInt("id")
	context.Writef(fmt.Sprintf("%d", userId))
}

func user_register(context *iris.Context){
	context.Writef("")
}

func main() {
	app := iris.New()
	app.Adapt(httprouter.New())
	app.HandleFunc("GET", "/", index)
	app.HandleFunc("GET", "/user/:id", user)
	app.HandleFunc("POST", "/user", user_register)
	app.Listen("127.0.0.1:3000")
}
