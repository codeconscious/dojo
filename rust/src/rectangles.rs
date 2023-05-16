#[derive(Debug)]
struct Rectangle {
    pub width: i32,
    pub height: i32,
    pub color: Rgb,
}

impl Rectangle {
    pub fn area(&self) -> i32 {
        self.height * self.width
    }

    pub fn can_contain(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }

    // Associated function (i.e., C# static method). Call with "::".
    pub fn square(size: i32) -> Self {
        Self {
            width: size,
            height: size,
            color: Rgb::new_blank(),
        }
    }
}

#[derive(Debug)]
pub struct Rgb {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
}

impl Rgb {
    pub fn as_text(&self) -> String {
        "R".to_owned()
            + &self.red.to_string()
            + &" G".to_owned()
            + &self.green.to_string()
            + &" B".to_owned()
            + &self.blue.to_string()
    }

    // A private method.
    fn new_blank() -> Self {
        Self {
            red: 0,
            green: 0,
            blue: 0,
        }
    }
}

pub fn run() {
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
    println!("rect1 is {:?}", rect1); // Single line
    println!("rect1 is {:#?}", rect1); // Multiple lines
    dbg!(&rect1); // Debug version of multi-line one

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
    println!(
        "The first rectangle {} contain the second.",
        if contained { "DOES" } else { "does NOT" }
    );

    let square = Rectangle::square(10);
}
