pub fn run() {
    let pair = SameTypePair {
        first: 4,
        second: 4,
        // array: [0, 1, 2, 3]
    };

    let pair2 = DifferingTypePair {
        first: 13,
        second: "よ！",
    };
}

struct SameTypePair<T> {
    first: T,
    second: T,
    // array: [T],
    /* ERROR:
       the size for values of type `[{integer}]` cannot be known at compilation time
       within `Pair<{integer}>`, the trait `Sized` is not implemented for `[{integer}]`
       structs must have a statically known size to be initialized
    */
}

struct DifferingTypePair<T, U> {
    first: T,
    second: U,
}

// Define methods only for certain generic types!
impl DifferingTypePair<i32, bool> {
    fn convert(&self) -> i32 {
        match self.second {
            true => return self.first * -1,
            _ => return self.first,
        }
    }
}

enum MultipleTypes<T, U> {
    FirstType(T),
    OtherType(U),
}

// Example: https://doc.rust-lang.org/stable/book/ch10-01-syntax.html

struct Point<X1, Y1> {
    x: X1,
    y: Y1,
}

impl<X1, Y1> Point<X1, Y1> {
    fn mixup<X2, Y2>(self, other: Point<X2, Y2>) -> Point<X1, Y2> {
        Point {
            x: self.x,
            y: other.y,
        }
    }
}

fn test() {
    let p1 = Point { x: 5, y: 10.4 };
    let p2 = Point { x: "Hello", y: 'c' };

    let p3 = p1.mixup(p2);

    println!("p3.x = {}, p3.y = {}", p3.x, p3.y);
}
