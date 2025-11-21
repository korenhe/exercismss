pub fn series(digits: &str, len: usize) -> Vec<String> {
    digits.chars().collect::<Vec<char>>().windows(len).map(|tuple| tuple.iter().collect::<String>()).collect::<Vec<String>>()
}
