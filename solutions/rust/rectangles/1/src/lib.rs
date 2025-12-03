fn count_of_two_lines(a: &str, b: &str) -> Vec<(usize,usize)> {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();

    let col_cnt = a.len();
    let mut res: Vec<(usize, usize)> = vec![];

    for i in 0..col_cnt {
        for j in i + 1..col_cnt {
            if a[i] == '+' && a[j] == '+' &&
               b[i] == '+' && b[j] == '+' {
                res.push((i, j));
            }
        }
    }

    res
}

fn connect(rect:(usize,usize,usize,usize), lines:&[&str]) ->bool {
    let x = rect.0;
    let y = rect.1;
    let a = rect.2;
    let b = rect.3;

    // (x,a), (x,b)
    // (y,a), (y,b)
    for i in a..b {
        let linex = lines[x].chars().collect::<Vec<_>>();
        let liney = lines[y].chars().collect::<Vec<_>>();
        if (linex[i] != '+' && linex[i] != '-') || (liney[i] != '+' && liney[i] != '-') {
            return false;
        }
    }

    for line in lines.iter().take(y).skip(x) {
        let linea = line.chars().nth(a).unwrap();
        let lineb = line.chars().nth(b).unwrap();
        if (linea != '+' && linea != '|') || (lineb != '+' && lineb != '|') {
            return false;
        }
    }

    true
}

pub fn count(lines: &[&str]) -> u32 {
    let mut res: Vec<(usize,usize,usize,usize)> = vec![];
    let row_cnt = lines.len();
    for i in 0..row_cnt {
        for j in i+1..row_cnt {
            let ls = count_of_two_lines(lines[i], lines[j]);
            res.append(&mut ls.iter().map(|(a,b)| (i,j,*a,*b)).collect::<Vec<_>>());
        }
    }

    // check each rectangle is connected
    let mut num = 0;
    for rect in res {
        if connect(rect, lines) {
            num += 1;
        }
    }

    num
}
