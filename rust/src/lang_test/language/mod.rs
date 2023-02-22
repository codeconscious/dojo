use super::char_types::*;

pub enum Language {
    English(EnglishCharType),
    Japanese(JapaneseCharType),
    Chinese,
}

pub fn lang_value(lang: Language) -> i32 {
    match lang {
        Language::English(EnglishCharType::UppercaseLetter) => -1,
        Language::English(EnglishCharType::LowercaseLetter) => -2,
        Language::Japanese(JapaneseCharType::Hiragana) => 1,
        Language::Japanese(JapaneseCharType::Katakana) => 2,
        Language::Japanese(JapaneseCharType::Kanji) => 3,
        _ => {
            println!("No supported value was submitted.");
            0
        }
    }
}
