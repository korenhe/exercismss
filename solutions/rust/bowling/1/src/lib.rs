#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    NotEnoughPinsLeft,
    GameComplete,
}

pub struct BowlingGame {
    frame: usize,
    records: Vec<Vec<u16>>,
    freethrows: u8,
}

impl BowlingGame {
    pub fn new() -> Self {
        BowlingGame {
            frame: 0,
            records: vec![vec![]; 10],
            freethrows: 2,
        }
    }

    pub fn roll(&mut self, pins: u16) -> Result<(), Error> {
        if self.freethrows == 0 && self.frame < 9 {
            self.frame += 1;
            self.freethrows = 2;
        }

        if self.frame == 10 || self.freethrows == 0 {
            return Result::Err(Error::GameComplete);
        }

        let record = &mut self.records[self.frame];
        let already_got: u16 = record.iter().sum();
        if self.frame < 9 {
            if already_got + pins > 10 {
                return Result::Err(Error::NotEnoughPinsLeft);
            }
        } else {
            if pins > 10 {
                return Result::Err(Error::NotEnoughPinsLeft);
            }

            let already: u16 = record
                .iter()
                .skip_while(|&&x| x == 10)
                .copied()
                .collect::<Vec<_>>()
                .iter()
                .sum();
            if already != 10 && already + pins > 10 {
                return Result::Err(Error::NotEnoughPinsLeft);
            }
        }
        record.push(pins);

        self.freethrows -= 1;

        if already_got + pins == 10 {
            if self.frame == 9 {
                if record.len() == 1 || (record.len() == 2 && pins != 0) {
                    self.freethrows += 1;
                }
            } else {
                self.freethrows = 2;
                self.frame += 1;
            }
        }

        Ok(())
    }

    fn getthrows(&self, start: usize, count: usize) -> u16 {
        let mut c: usize = count;
        let mut sum: u16 = 0;
        let mut cursor = start + 1;
        while c > 0 && cursor < 10 {
            let record = &self.records[cursor];
            for item in record.iter().take(c.min(record.len())) {
                sum += item;
                c -= 1;
            }
            cursor += 1;
        }
        sum
    }

    pub fn score(&self) -> Option<u16> {
        if self.freethrows > 0 || self.frame != 9 {
            return None;
        }

        let mut score: u16 = 0;
        for i in 0..9 {
            let record = &self.records[i];
            score += match record.len() {
                1 => 10 + self.getthrows(i, 2),
                2 => {
                    let sum = record.iter().sum();
                    if sum == 10 {
                        10 + self.getthrows(i, 1)
                    } else {
                        sum
                    }
                }
                _ => return None,
            }
        }
        // last
        score += self.records[9].iter().sum::<u16>();
        Some(score)
    }
}
