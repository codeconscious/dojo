#[derive(Debug)]
pub struct Rectangle {
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

    // Note that this method is private.
    fn new_blank() -> Self {
        Self {
            red: 0,
            green: 0,
            blue: 0,
        }
    }
}
