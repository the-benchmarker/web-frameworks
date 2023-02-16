package main

import (
	"fmt"
	"gitee.com/aurora-engine/aurora"
)

func main() {
	err := aurora.Run(&Server{aurora.New(aurora.Debug())})
	if err != nil {
		fmt.Println(err)
		return
	}
}
