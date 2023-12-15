pub fn run() {
    #[derive(PartialEq)] // Enables `==` below.
    struct MyStruct<'a> {
        text: &'a str,
    }
    let a = MyStruct { text: "Hi!" };
    let b = &a; // Immutable reference to what `a` contains
    let c = &a; // Immutable reference to what `a` contains
    let d = &b; // Immutable reference via `b` to the reference `a` contains (&&)
    println!("b == c:   {}", a == *b); // true
    println!("b == c:   {}", b == c); // true
    println!("b == *d:  {}", b == *d); // true
    println!("a == **d: {}", a == **d); // true

    let mut a = MyStruct { text: "2nd!" };
    let b = &mut a; // Mutable ref to what `a` contains
    b.text = "Updated text";
    println!("b.text == {}", b.text); // Updated text

    // Dereferencing
    let x = 5; // i32
    let y = &x; // &i32
    assert_eq!(x, 5); // True, so nothing happens
    // assert_eq!(y, 5); // ERROR: can't compare `{integer}` with `&{integer}`
    assert_eq!(*y, 5); // Accesses the int32 directly// True, so nothing happens

    // Using Box<T> like a reference
    let x = 5;
    let y = Box::new(x);
    assert_eq!(5, x);
    assert_eq!(5, *y);

    // Setup _without_ Deref
    struct MyBox<T>(T);
    impl<T> MyBox<T> {
        fn new(x: T) -> MyBox<T> {
            MyBox(x)
        }
    }
    //
    let x = 5;
    let y = MyBox::new(x);

    assert_eq!(5, x);
    // assert_eq!(5, *y); // type `MyBox<{integer}>` cannot be dereferenced

    // Setup _with_ Deref
    struct MyBetterBox<T>(T);
    impl<T> MyBetterBox<T> {
        fn new(x: T) -> MyBetterBox<T> {
            MyBetterBox(x)
        }
    }
    use std::ops::Deref;
    impl<T> Deref for MyBetterBox<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
}
