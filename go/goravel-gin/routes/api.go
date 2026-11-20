package routes

import (
	"github.com/goravel/framework/facades"

	"goravel/app/http/controllers"
)

func Api() {
	apiController := controllers.NewApiController()
	facades.Route().Get("/", apiController.Index)
	facades.Route().Get("/user/{id}", apiController.ShowUser)
	facades.Route().Post("/user", apiController.CreateUser)
}
