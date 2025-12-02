pub fn rotate(input: &str, key: u8) -> String {
    input.chars().map(|x| {
        if x.is_uppercase() {
            ('A' as u8 + (x as u8 - 'A' as u8 + key) % 26) as char
        } else if x.is_lowercase() {
            ('a' as u8 + (x as u8 - 'a' as u8 + key) % 26) as char
        } else {
            x
        }
    }).collect::<String>()
}
