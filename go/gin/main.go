package main

import (
	"github.com/gin-gonic/gin"
)

func main() {
	gin.SetMode(gin.ReleaseMode)

	r := gin.New()

	r.GET("/", func(c *gin.Context) {
		c.Writer.Write([]byte(""))
	})

	r.GET("/user/:name", func(c *gin.Context) {
		name := c.Params.ByName("name")
		c.Writer.Write([]byte(name))
	})

	r.POST("/user", func(c *gin.Context) {
		c.Writer.Write([]byte(""))
	})

	r.Run(":3000")
}
