/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    if !code.chars().all(|x| x.is_ascii_digit() || x.is_whitespace()) {
        return false;
    }

    let l1 = code.chars()
        .rev()
        .filter(|x| x.is_ascii_digit())
        .map(|x| {
            x as u8 - b'0'
        }).collect::<Vec<u8>>();

    if l1.len() < 2 {
        return false;
    }

    l1.into_iter().enumerate()
        .map(|(i, x)| {
            let mut y = x;
            if i % 2 == 1 {
                y *= 2;
                if y > 9 {
                    y -= 9;
                }
            }
            y as u32
        })
        .sum::<u32>() % 10 == 0
}
