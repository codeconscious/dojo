pub fn add_all(numbers: Option<(i32, i32, i32)>) -> Option<i32> {
    match numbers {
        None => None,
        Some((a, b, c)) => Some(a + b + c),
    }
}

pub fn average(first: f32, second: f32, third: f32) -> f32 {
    (first + second + third) / 3.0
}
