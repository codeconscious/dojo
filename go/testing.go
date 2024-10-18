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

// Taken wholesale from https://stackoverflow.com/a/50025091/11767771.
// Golang doesn't have its own `map`ping function yet!
func Map(vs []string, f func(string) string) []string {
	vsm := make([]string, len(vs))
	for i, v := range vs {
		vsm[i] = f(v)
	}
	return vsm
}
