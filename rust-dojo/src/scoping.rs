pub fn run() {
    println!("SCOPING and SHADOWING:");

    let sum = add(6, 7, 23);
    println!("  outer sum == {sum}");

    // Shadowing via inner scope
    {
        let sum = add(50, 30, 1000);
        println!("  inner sum == {sum}");
    }

    println!("  outer sum == {sum}");
}

fn add(first: i32, second: i32, third: i32) -> i32 {
    first + second + third
}
