// The code below is a stub. Just enough to satisfy the compiler.
// In order to pass the tests you can add-to or change any of this code.

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    InvalidRowCount(usize),
    InvalidColumnCount(usize),
}

pub fn convert(input: &str) -> Result<String, Error> {
    let map = [
        " _ | ||_|   ",
        "     |  |   ",
        " _  _||_    ",
        " _  _| _|   ",
        "   |_|  |   ",
        " _ |_  _|   ",
        " _ |_ |_|   ",
        " _   |  |   ",
        " _ |_||_|   ",
        " _ |_| _|   ",
    ];

    let digits = input.split('\n').collect::<Vec<_>>();
    if digits.len() % 4 > 0 {
        return Err(Error::InvalidRowCount(digits.len()));
    }

    let digits = digits
        .chunks(4)
        .map(|rows| {
            let mut digits_in_row = Vec::<Vec<char>>::new();
            let num = rows[0].len() / 3;
            digits_in_row.resize(num, vec![]);

            for row in rows.iter().take(4) {
                if row.len() % 3 > 0 {
                    return Err(Error::InvalidColumnCount(row.len()));
                }

                row.chars()
                    .collect::<Vec<_>>()
                    .chunks(3)
                    .enumerate()
                    .for_each(|(index, cols)| {
                        digits_in_row[index].append(&mut cols.to_vec());
                    });
            }
            Ok(digits_in_row
                .iter()
                .map(|x| x.iter().collect::<String>())
                .collect::<Vec<_>>())
        })
        .collect::<Result<Vec<_>, Error>>()?;

    Ok(digits
        .iter()
        .enumerate()
        .flat_map(|(rn, rowdigit)| {
            let mut rows = rowdigit
                .iter()
                .map(|digit| {
                    let pos = map.iter().position(|x| x == digit);
                    if let Some(pos) = pos {
                        (b'0' + pos as u8) as char
                    } else {
                        '?'
                    }
                })
                .collect::<Vec<_>>();
            if rn < digits.len() - 1 {
                rows.push(',');
            }
            rows
        })
        .collect::<String>())
}
