pub fn is_armstrong_number(num: u32) -> bool {
    let mut n = num;
    let mut nlist = vec![];
    while n > 0 {
        nlist.push(n % 10);
        n /= 10;
    }
    num == nlist.iter().map(|x| x.pow(nlist.len() as u32)).sum()
}
