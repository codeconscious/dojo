pub fn run() {
    use regex::Regex;
    // Reference: https://docs.rs/regex/latest/regex/
    use regex::RegexSet;

    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    assert!(re.is_match("2014-01-01"));

    let set = RegexSet::new(&[
        r"\w+", r"\d+", r"\pL+", r"foo", r"bar", r"barfoo", r"foobar",
    ])
    .unwrap();

    // Iterate over and collect all of the matches.
    let matches: Vec<_> = set.matches("foobar").into_iter().collect();
    assert_eq!(matches, vec![0, 2, 3, 4, 6]);

    // TEST: Gather success

    // You can also test whether a particular regex matched:
    let matches = set.matches("foobar");
    assert!(!matches.matched(5));
    assert!(matches.matched(6));
}
