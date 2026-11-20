package config

import (
	"github.com/goravel/framework/facades"
)

func init() {
	config := facades.Config()
	config.Add("cache", map[string]any{
		"default": config.Env("CACHE_STORE", "memory"),
		"stores": map[string]any{
			"memory": map[string]any{
				"driver": "memory",
			},
		},
		"prefix": config.GetString("APP_NAME", "goravel") + "_cache",
	})
}
