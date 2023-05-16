use rand::Rng;
use std::io;
use std::io::Write;

pub fn play(show_hints: bool) {
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
