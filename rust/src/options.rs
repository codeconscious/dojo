use crate::math;

pub fn run() {
    let some_sum = math::add_all(Some((50, 100, 400)));
    let none_sum = math::add_all(None);

    // A shorter syntax in exchange for the loss of exhaustive pattern matching.
    if let Some(sum) = some_sum {
        println!("The sum is {}!", sum);
    }

    if let Some(sum) = none_sum {
        println!("The sum is {}, but this line will not print!", sum);
    }
}
