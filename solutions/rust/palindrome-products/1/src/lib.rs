use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Palindrome {
    value: u64,
    min: u64,
    max: u64,
}

impl Palindrome {
    pub fn value(&self) -> u64 {
        self.value
    }

    pub fn into_factors(self) -> HashSet<(u64, u64)> {
        let mut res: HashSet<(u64, u64)> = HashSet::<_>::new();

        for i in 1.max(self.min)..=self.value.min(self.max) {
            if self.value % i == 0 {
                let other = self.value / i;
                if i <= other && other <= self.max && other >= self.min {
                    res.insert((i, other));
                }
            }
        }
        res
    }
}

fn is_palindrom(value: u64) -> bool {
    let mut res: Vec<u8> = vec![];
    let mut value = value;
    while value > 0 {
        res.push((value % 10) as u8);
        value /= 10;
    }
    res == res.iter().rev().cloned().collect::<Vec<_>>()
}

pub fn palindrome_products(min: u64, max: u64) -> Option<(Palindrome, Palindrome)> {
    let mut lmin: Option<u64> = None;

    for i in min..=max {
        for j in i..=max {
            let mul = i * j;
            if let Some(lmin) = lmin {
                if mul > lmin {
                    break;
                }
            }
            if is_palindrom(mul) {
                if let Some(x) = lmin {
                    lmin = Some(x.min(mul));
                } else {
                    lmin = Some(mul);
                }
            }
        }
    }

    let mut lmax: Option<u64> = None;
    for i in (min..=max).rev() {
        for j in (i..=max).rev() {
            let mul = i * j;
            if let Some(lmax) = lmax {
                if mul < lmax {
                    break;
                }
            }
            if is_palindrom(mul) {
                if let Some(x) = lmax {
                    lmax = Some(x.max(mul));
                } else {
                    lmax = Some(mul);
                }
            }
        }
    }

    match (lmin, lmax) {
        (Some(lmin), Some(lmax)) => Some((
            Palindrome {
                value: lmin,
                min,
                max,
            },
            Palindrome {
                value: lmax,
                min,
                max,
            },
        )),
        _ => None,
    }
}
