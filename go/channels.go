package main

import "fmt"

func inc(i int, c chan int) {
	c <- i + 1 // `<-`` is the "send" operator when a channel appears on the left.
}

func learnConcurrency() {
	c := make(chan int)

	// Start three concurrent goroutines. Numbers will be incremented
	// concurrently, perhaps in parallel if the machine is capable and
	// properly configured. All three send to the same channel.
	go inc(0, c) // go is a statement that starts a new goroutine.
	go inc(10, c)
	go inc(-805, c)

	// Read three results from the channel and print them out.
	// There is no telling in what order the results will arrive!
	fmt.Println(<-c, <-c, <-c) // channel on right, `<-`` is "receive" operator.

	cs := make(chan string)
	ccs := make(chan chan string) // A channel of string channels.
	go func() { c <- 84 }()
	go func() { cs <- "wordy" }()

	// Select has syntax like a switch statement, but each case involves
	// a channel operation. It selects a case at random out of the cases
	// that are ready to communicate.
	select {
	case i := <-c: // The value received can be assigned to a variable,
		fmt.Printf("it's a %T", i)
	case <-cs: // or the value received can be discarded.
		fmt.Println("it's a string")
	case <-ccs: // Empty channel, not ready for communication.
		fmt.Println("didn't happen.")
	}

	// At this point a value was taken from either c or cs. One of the two
	// goroutines started above has completed, the other will remain blocked.
}
