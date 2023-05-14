#![allow(dead_code)]
#![allow(unused_variables)]

mod collections;
mod generics;
mod lang_test;
mod math;
mod options;
mod rectangle;
mod regex;
mod scoping;
mod boxes;

use rand::Rng;
use rectangle::*;
// use lang_test::language::{self, Language};
// use lang_test::char_types::{JapaneseCharType, EnglishCharType};
use lang_test::{char_types::*, language::*};
use std::io;
use std::io::Write;

fn main() {
    // misc();
    // string_experiments();
    // scoping::run();
    // options::run();
    // rectangle_practice();
    // guessing_game(false);
    // collections::run();
    // regex::run();
    boxes::with_and_without_boxing();
}

// Where unfiled code lives.
fn misc() {
    let average = math::average(50.0, 20.3, 33.9);
    println!("average == {average}");

    let parsed: u32 = "1000".parse().expect("Couldn't parse");
    println!("parsed == {parsed}");
    // let invalidParse: u32 = "1000a".parse().expect("Couldn't parse");

    let tup: (i32, i64, char) = (32, 64, 'c');
    println!(
        "Tuple: {}, {}, and {}",
        tup.0.to_string(),
        tup.1, // .to_string() is optional
        tup.2
    );

    let lang = Language::English(EnglishCharType::UppercaseLetter);
    let lang_value: i32 = lang_value(lang);
    println!("lang_value == {lang_value}");

    // Maps, etc.
    // let items = vec![1, 2,3];
    // let new_items = items.

    // Anonymous struct!
    let a: (i32, i32) = (10, 10);
}

fn string_experiments() {
    println!("STRINGS:");

    const A: &str = "This is a string!";
    println!("  {A}");

    let mut b = "ハロー！ I am a mutable string.";
    println!("  {b}");
    b = "ハロー！（改)";
    println!("  {b}");

    let mut c = String::from("こんにちは");
    c.push_str("、皆さん！"); // c = "a"はダメ;
    println!("  {c}");
    let c2 = c;

    // The next line does not compile because `c` was "moved" to `c2`, so cannot be "borrowed" here.
    // To remedy this, use 'c.clone()` above instead of just `c`. Could be expensive, though.
    // println!("{c}");
}

fn rectangle_practice() {
    let rect1 = Rectangle {
        width: 5,
        height: 7,
        color: Rgb {
            red: 70,
            green: 80,
            blue: 90,
        },
    };
    println!(
        "rect's area is {}, and its color is {}.",
        rect1.area(),
        rect1.color.as_text()
    );
    println!("rect1 is {:?}", rect1); // Unformatted
    println!("rect1 is {:#?}", rect1); // Formatted
    dbg!(&rect1);

    let rect2 = Rectangle {
        width: 3,
        height: 3,
        color: Rgb {
            red: 0,
            green: 0,
            blue: 0,
        },
    };
    let contained = rect1.can_contain(&rect2);
    println!("{contained}");

    let square = Rectangle::square(10);
}

// fn combine_strings(first: &str, second: &str) -> &str {
//     first + second
// }

fn guessing_game(show_hints: bool) {
    let range_floor: i8 = 1;
    let range_ceiling: i8 = 100;

    print!("\x1B[2J"); // Clears the screen.
    println!("HOW TO PLAY:");
    println!(
        "　{}から{}までで、好きな数字を入力して、",
        range_floor, range_ceiling
    );
    println!("　ランダムに決められた『シークレットナンバー』を");
    println!("　当ててみてください。");

    let random_number = rand::thread_rng().gen_range(range_floor..=range_ceiling);
    if show_hints {
        println!("(The number is {}.)", random_number);
    }

    let mut attempts = 0;

    loop {
        let mut guess = String::new(); // Must be within this loop.
        print!("＞ ");
        io::stdout().flush().unwrap(); // The previous one will not display immediately with this.
        std::io::stdin()
            .read_line(&mut guess)
            .expect("Failed to read from stdin");

        // let guess: i16 = guess.trim().parse().expect("Please type a number!");
        let guess: i8 = match guess.trim().parse() {
            // Reinstantiation!
            Ok(num) => num,
            Err(_) => continue,
        };
        // println!("You entered {}.", guess);

        match guess.cmp(&random_number) {
            std::cmp::Ordering::Less => println!("低すぎるぞ"),
            std::cmp::Ordering::Greater => println!("高すぎるぞ"),
            std::cmp::Ordering::Equal => {
                println!("ピンポ〜ン！おめでとうございます!🎉");
                println!("{}回で当てました。", attempts);
                break;
            }
        }

        attempts = attempts + 1;
    }
}

fn references() {
    struct MyStruct<'a> {
        text: &'a str,
    }
    let a = MyStruct { text: "Hi!" };
    let b = &a; // Immutable reference to what `a` contains
    let c = &a; // Immutable reference to what `a` contains
    let d = &b; // Immutable reference via `b` to what `a` contains
                // println!("{}", d.to_string()); // TODO: Figure out

    let mut a = MyStruct { text: "2nd!" };
    let b = &mut a; // Mutable ref to what `a` contains

    // Dereferencing
    let x = 5; // i32
    let y = &x; // &i32
    assert_eq!(x, 5);
    // assert_eq!(y, 5); // ERROR: can't compare `{integer}` with `&{integer}`
    assert_eq!(*y, 5); // Accesses the int32 directly

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
