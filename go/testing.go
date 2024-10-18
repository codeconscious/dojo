package main // Declares an executable rather than a library.

import (
	"fmt"
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
		1, "Jean", "Grey-Summers", time.Now(),
		[]power{telepathy, telekinesis}, xMen,
	}
	fmt.Println(SummarizePerson((jean)))
}

func GreetPerson(name string) string {
	return "こんにちは、" + name + "さん"
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
