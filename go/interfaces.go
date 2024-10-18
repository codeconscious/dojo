package main

import "fmt"

type Stringifier interface {
	String() string
}

type pair struct {
	x, y int // Two fields of the same type.
}

// Defines a method on type pair, which now apparently implements Stringer
// because Pair has defined all the methods in the interface.
func (p pair) String() string { // `p` is the "receiver"
	return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func learnInterfaces() {
	p := pair{3, 4} // A "struct literal"
	fmt.Println(p.String())
	var i Stringifier
	i = p // Valid because `pair` implements Stringifier
	fmt.Println(i.String())

	// Functions in the fmt package call the String() method to ask an object
	// for a printable representation of itself.
	fmt.Println(p)
	fmt.Println(i)
}
