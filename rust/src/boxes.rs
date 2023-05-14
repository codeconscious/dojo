pub fn with_and_without_boxing() {
    let a = 123;
    let b = Box::new(123);

    println!("{}", a + *b);
}
