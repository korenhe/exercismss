fn say1(n: u8) ->String {
    let list = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "ninteen"];
    list[n as usize].to_string()
}

fn say2(n: u8) -> String {
    let mut res : String = "".to_string();
    if n < 20 {
        res += &say1(n);
    } else {
        let m = n / 10;
        let list = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"];
        let k = n % 10;
        res += list[m as usize];
        if k > 0 {
            res += &format!("-{}", say1(k));
        }
    }
    res
}

fn say3(n: u16) -> String {
    let mut n = n;
    let mut res: String = "".to_string();
    if n / 100 > 0 {
        res += &format!("{} hundred", say2((n/100) as u8));
    }

    let m = n % 100;
    n /= 100;
    if m > 0 {
        if n > 0 {
            res += " ";
        }
        res += &say2(m as u8);
    }

    res
}

pub fn encode(n: u64) -> String {
    let mut n = n;
    if n == 0 {
        return "zero".to_string();
    }

    let triunit = ["", "thousand", "million", "billion", "trillion", "quadrillion", "quintillion"];
    let mut triindex = 0;
    let mut res: Vec<String> = vec![];

    while n > 0 {
        let m = n % 1000;
        if m > 0 {
            if triindex > 0 {
                res.push(format!("{} {}", say3(m as u16), triunit[triindex]));
            } else {
                res.push(say3(m as u16));
            }
        }
        triindex += 1;
        n /= 1000;
    }

    res.iter().rev().cloned().collect::<Vec<String>>().join(" ")
}
