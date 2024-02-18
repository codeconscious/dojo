// pub mod quadratic;
pub fn start() -> () {
    println!("{}", friendly_output(0.0, 3.0, 1.0));
    println!("{}", friendly_output(1.0, 3.0, 1.0));
    println!("{}", friendly_output(1.0, 2.0, 1.0));
    println!("{}", friendly_output(1.0, 2.0, 3.0));
}

enum EquationType {
    NotQuadratic,
    NoRoots,
    OneRoot { positive: f32 },
    TwoRoots { positive: f32, negative: f32 },
}

fn calculate_type(a: f32, b: f32, c: f32) -> EquationType {
    if a == 0.0 {
        return EquationType::NotQuadratic;
    }

    let discrimant = b.powf(2.0) - (4.0 * a * c); // b^2 - 4ac
    let root_positive = (-b + discrimant.sqrt()) / (2.0 * a); // quad eq plus sqrt
    let root_negative = (-b - discrimant.sqrt()) / (2.0 * a); // quad eq minus sqrt

    match discrimant {
        d if d > 0.0 => EquationType::TwoRoots {
            positive: root_positive,
            negative: root_negative,
        },
        d if d == 0.0 => EquationType::OneRoot {
            positive: root_positive,
        },
        _ => EquationType::NoRoots,
    }
}

fn friendly_output(a: f32, b: f32, c: f32) -> String {
    let result = calculate_type(a, b, c);

    match result {
        EquationType::NotQuadratic => "The equation is not quadratic!".to_owned(),
        EquationType::NoRoots => "The equation has no real roots.".to_owned(),
        EquationType::OneRoot { positive } => format!("The equation has one root: {}.", positive),
        EquationType::TwoRoots { positive, negative } => {
            format!("The equation has two roots: {} and {}.", positive, negative)
        }
    }
}
