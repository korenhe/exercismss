fn update(garden: &[&str], row: usize, col: usize, width:usize, height:usize, result: &mut [Vec<i32>]) {
    (row.saturating_sub(1)..=(row+1).min(height - 1)).for_each(|r| {
        (col.saturating_sub(1)..=(col+1).min(width - 1)).for_each(|c| {
            let old = garden[r].chars().nth(c)
                .unwrap_or_else(|| panic!("old ({} {}) should be a char", r,c));
            if old != '*' {
                result[r][c] += 1;
            }
        });
    });
}

pub fn annotate(garden: &[&str]) -> Vec<String> {
    let height = garden.len();
    if height == 0 {return [].to_vec();}
    let width = garden[0].len();
    if width == 0 {return vec!["".to_string()];}

    let mut result = vec![vec![0; width]; height];

    garden.iter().enumerate().for_each(|(rid, row)| {
        row.chars().enumerate().for_each(|(cid, x)| {
            if x == '*' {
                result[rid][cid] = -1;
                update(garden, rid, cid, width, height, &mut result);
            }
        });
    }
    );
    result.iter().map(|row| {
        row.iter().map(|x| {
            match *x {
                y if y > 0 => char::from(b'0' + y as u8),
                0 => ' ',
                _ => '*',
            }
        }).collect::<String>()
    }).collect()
}
