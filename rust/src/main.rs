#![allow(dead_code)]
#![allow(unused_variables)]

mod boxes;
mod collections;
mod generics;
mod guessing_game;
mod lang_test;
mod math;
mod options;
mod rectangles;
mod references;
mod regex;
mod scoping;

// use lang_test::language::{self, Language};
// use lang_test::char_types::{JapaneseCharType, EnglishCharType};
use lang_test::{char_types::*, language::*};

fn main() {
    // misc();
    // string_experiments();
    // scoping::run();
    // options::run();
    // guessing_game::play(false);
    // collections::run();
    // regex::run();
    // boxes::with_and_without_boxing();
    // rectangles::run();
    references::run();
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

fn combine_strings(first: &str, second: &str) -> String {
    first.to_owned() + second
}
