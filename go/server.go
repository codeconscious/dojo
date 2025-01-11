package main

import (
	"fmt"
	"io"
	"net/http"
)

func learnWebProgramming() {
	// First parameter is the TCP address to listen to.
	// Second parameter is an interface, specifically http.Handler.
	go func() {
		err := http.ListenAndServe(":8080", nil)
		fmt.Println(err) // don't ignore errors
	}()

	requestServer()
}

func requestServer() {
	resp, err := http.Get("http://localhost:8080")
	fmt.Println(err)
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	fmt.Printf("\nWebserver said: `%s`", string(body))
}
