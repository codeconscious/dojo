pub fn run() {
    arrays();
    vectors();
    strings();
    hash_map_basics();
}

fn arrays() {
    let arr = [1_000, 2_000, 3_000];
    for i in arr {
        println!("{i}")
    }
}

fn vectors() {
    let v: Vec<i32> = Vec::new();
    let mut v = vec![0, 1, 2, 3]; // No type declaration needed.
    v.push(4);

    // Two ways to get
    let third: i32 = v[3];
    let third: &i32 = &v[3];
    let third: Option<&i32> = v.get(3);
    match third {
        Some(x) => {
            println!("Found \"{x}\"");
        }
        None => {
            println!("Nothing found")
        }
    };

    // Iteration
    for i in &v {
        // `&mut v` works too
        println!("{i}")
    }

    for i in &mut v {
        // `&mut v` works too
        *i += 300; // Derefence operator; permanently mutates the vector
    }

    for i in &v {
        // `&mut v` works too
        println!("{i}")
    }

    // Enums
    enum SpreadsheetCell {
        Int(i32),
        Float(f64),
        Text(String),
    }

    let row = vec![
        SpreadsheetCell::Int(3),
        SpreadsheetCell::Text(String::from("blue")),
        SpreadsheetCell::Float(10.12),
    ];
}

fn strings() {
    let s = String::new();
    let data: &str = "initial contents";
    let s: String = data.to_string(); // Usable on types that implement the Display trait

    // Identical:
    let s = "initial contents".to_string();
    let s = String::from("initial contents");

    // Mutation
    let mut s = String::from("こんに");
    s.push_str("ちは");
    s.push('！');

    let s1 = String::from("Hello, ");
    let s2 = String::from("world!");
    let s3 = s1 + &s2; // s1 has been moved here and can no longer be used

    let s1 = String::from("tic");
    let s2 = String::from("tac");
    let s3 = String::from("toe");
    let s = format!("{s1}-{s2}-{s3}"); // format! does not take ownership
}

fn hash_map_basics() {
    use std::collections::HashMap;

    let mut scores: HashMap<String, i32> = HashMap::new();
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    let mut scores = HashMap::new();

    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    let team_name = String::from("Blue");
    let score = scores.get(&team_name).copied().unwrap_or(0);
    // let score = scores.get(&team_name).copied();
    // match score {
    //     Some(_) => println!("Found"),
    //     None => println!("Not found")
    // }
    for (key, value) in scores {
        println!("{key}: {value}");
    }

    // Update conditionally
    let mut scores = HashMap::new();
    scores.insert(String::from("Blue"), 10);
    scores.entry(String::from("Yellow")).or_insert(50);
    scores.entry(String::from("Blue")).or_insert(50);
    println!("{:?}", scores); // {"Blue": 10, "Yellow": 50}

    // Update based on existing values
    let text = "hello world wonderful world";
    let mut map = HashMap::new();
    for word in text.split_whitespace() {
        let count = map.entry(word).or_insert(0);
        *count += 1;
    }
    println!("{:?}", map); // {"world": 2, "hello": 1, "wonderful": 1}
}

fn collection_exercise_1() {
    // Given a list of integers, use a vector and return the median
    // (when sorted, the value in the middle position) and mode
    // (the value that occurs most often; a hash map will be helpful here) of the list.
}

fn collection_exercise_2() {
    // Convert strings to pig latin. The first consonant of each word is moved
    // to the end of the word and “ay” is added, so “first” becomes “irst-fay.”
    // Words that start with a vowel have “hay” added to the end instead (“apple”
    // becomes “apple-hay”). Keep in mind the details about UTF-8 encoding!
}

fn collection_exercise_3() {
    // Using a hash map and vectors, create a text interface to allow a user to
    // add employee names to a department in a company. For example, “Add Sally to
    // Engineering” or “Add Amir to Sales.” Then let the user retrieve a list of
    // all people in a department or all people in the company by department, sorted alphabetically.
}
