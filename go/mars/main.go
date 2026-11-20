//go:generate go run github.com/roblillack/mars/cmd/mars-gen register-controllers .

package main

import (
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

func main() {
	RegisterControllers()

	mars.HttpAddr = ":3000"
	mars.RoutesFile = "routes.conf"
	mars.DisableCSRF = true
	mars.SetupRouter()

	mars.Run()
}
