package main

import (
	"net/http"

	"github.com/gin-gonic/gin"
)

func main() {
	gin.SetMode(gin.ReleaseMode)
	
	r := gin.New()

	r.GET("/", func(c *gin.Context) {
		c.String(http.StatusOK, "")
	})

	r.GET("/user/:name", func(c *gin.Context) {
		name := c.Params.ByName("name")
		c.String(http.StatusOK, name)
	})

	r.POST("/user", func(c *gin.Context) {
		c.String(http.StatusOK, "")
	})

	r.Run(":3000")
}
