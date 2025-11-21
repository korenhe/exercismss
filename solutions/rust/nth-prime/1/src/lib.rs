fn is_prime(n: u32) -> bool {
    !(2..=n.isqrt()).any(|x| n % x == 0)
}

pub fn nth(n: u32) -> u32 {
    let mut count = 0;
    let mut current = 1;
    while count < n + 1 {
        current += 1;
        if is_prime(current) {
            count += 1;
        }
    }
    current
}
