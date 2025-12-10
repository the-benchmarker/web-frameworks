package controllers

import (
	"github.com/goravel/framework/contracts/http"
)

type ApiController struct {
	// Dependent services
}

func NewApiController() *ApiController {
	return &ApiController{
		// Inject services
	}
}

func (r *ApiController) Index(ctx http.Context) http.Response {
	return ctx.Response().Success().String("")
}

func (r *ApiController) ShowUser(ctx http.Context) http.Response {
	return ctx.Response().Success().String(ctx.Request().Input("id"))
}

func (r *ApiController) CreateUser(ctx http.Context) http.Response {
	return ctx.Response().Success().String("")
}
