pub fn rotate(input: &str, key: u8) -> String {
    input.chars().map(|x| {
        if x.is_uppercase() {
            (b'A' + (x as u8 - b'A' + key) % 26) as char
        } else if x.is_lowercase() {
            (b'a' + (x as u8 - b'a' + key) % 26) as char
        } else {
            x
        }
    }).collect::<String>()
}
