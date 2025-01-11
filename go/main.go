/*
	Run using `go run`.
	Format using `go fmt`.
*/

package main // Declares an executable rather than a library.

import (
	"fmt"
	"os"
	"strings"
	"time"
)

type power struct {
	ID          int
	Description string
}

type team struct {
	ID   int
	Name string
}

type person struct {
	ID         int
	GivenName  string
	FamilyName string
	AddedTime  time.Time
	Powers     []power
	Team       team
}

func main() {
	fmt.Println(GreetPerson("四郎"))

	opticBlasts := power{1, "optic blasts"}
	xMen := team{1, "X-Men"}
	scott := person{
		1, "Scott", "Summers", time.Now(),
		[]power{opticBlasts}, xMen,
	}
	fmt.Println(SummarizePerson((scott)))

	telepathy := power{2, "telepathy"}
	telekinesis := power{3, "telekinesis"}
	jean := person{
		2, "Jean", "Grey-Summers", time.Now(),
		[]power{telepathy, telekinesis}, xMen,
	}
	fmt.Println(SummarizePerson((jean)))

	people := map[string]person{"cyke": scott, "jean": jean}

	betsy := person{2, "Betsy", "Braddock", time.Now(), []power{telepathy}, xMen}
	people["betsy"] = betsy
	// people["bobby"] // Returns the default value (!) -- `0` in this case.
	if val, ok := people["bobby"]; ok {
		fmt.Println("Key 'bobby' exists with value: ", val)
	}

	// Tip: Arrays have value semantics.
	// arr := [...]int{10, 20, 30} // Array of type `[3]int` (The `3` is part of the type!)
	slice := []int{10, 20, 30}
	head, _ := EdgeValues(slice)
	fmt.Println(head)

	file, _ := os.Create("test.test") // Ignores the error.
	fmt.Fprint(file, "自分、不器用ですから")
	file.Close()

	// Function literals are closures.
	isBiggerThan := func() bool {
		return slice[1] > 10
	}
	fmt.Println("答えは", isBiggerThan())

	fmt.Println("Add + double two numbers: ",
		func(a, b int) int {
			return (a + b) * 2
		}(10, 2))

	Defer()
	learnConcurrency()
	learnWebProgramming()
}

// Declare variables in the method signature!
func GreetPerson(name string) (greeting string) {
	greeting = "こんにちは、" + name + "さん"
	return // No need for variable name here!
}

func SummarizePerson(p person) string {
	return fmt.Sprintf("%s %s (%s) has the power(s) of %s.",
		p.GivenName,
		p.FamilyName,
		p.Team.Name,
		SummarizePowers(p.Powers))
}

func SummarizePowers(powers []power) string {
	var descriptions []string

	// `for` is the only loop statement. `break` and `continue` exist.
	for _, name := range powers {
		descriptions = append(descriptions, name.Description)
	}
	return strings.Join(descriptions, " and ")
}

// Source: https://stackoverflow.com/a/72498530/11767771.
// (Golang's standard library doesn't have its own `Map` function yet...)
func Map[T, V any](ts []T, fn func(T) V) []V {
	result := make([]V, len(ts))
	for i, t := range ts {
		result[i] = fn(t)
	}
	return result
}

// Functions can return multiple values:
func EdgeValues(arr []int) (head, tail int) {
	return arr[0], arr[len(arr)-1]
}

// Deferred function calls are pushed to a stack. Often used for closing files.
func Defer() (ok bool) {
	defer fmt.Println("deferred statements execute in reverse (LIFO) order.")
	defer fmt.Println("\nThis line is being printed first because")
	return true
}
