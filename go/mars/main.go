package main

import (
	"reflect"

	"github.com/roblillack/mars"
)

type App struct {
	*mars.Controller
}

func (a App) Index() mars.Result {
	return a.RenderText("")
}

func (a App) GetUser(id string) mars.Result {
	return a.RenderText(id)
}

func (a App) PostUser() mars.Result {
	return a.RenderText("")
}

// RegisterControllers will register all existing controllers to the server, so that the router
// will be able to resovle the actions and their arguments. Typically you'd use go generate
// for generating the registration code like this:
//   go:generate go run github.com/roblillack/mars/cmd/mars-gen register-controllers .
// For the sake of this example, the generated code is added to here, to have everything in one file.
func RegisterControllers() {
	mars.RegisterController((*App)(nil),
		[]*mars.MethodType{
			&mars.MethodType{
				Name: "Index",
				Args: []*mars.MethodArg{},
			},
			&mars.MethodType{
				Name: "GetUser",
				Args: []*mars.MethodArg{
					&mars.MethodArg{Name: "id", Type: reflect.TypeOf((*string)(nil))},
				},
			},
			&mars.MethodType{
				Name: "PostUser",
				Args: []*mars.MethodArg{},
			},
		})
}

func main() {
	RegisterControllers()

	mars.HttpAddr = ":3000"
	mars.RoutesFile = "routes.conf"
	mars.SetupRouter()

	mars.Run()
}
