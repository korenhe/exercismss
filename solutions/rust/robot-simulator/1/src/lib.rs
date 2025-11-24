use num_enum::TryFromPrimitive;
use std::convert::TryFrom;

// The code below is a stub. Just enough to satisfy the compiler.
// In order to pass the tests you can add-to or change any of this code.

#[repr(u8)]
#[derive(PartialEq, Eq, Debug, TryFromPrimitive)]
pub enum Direction {
    North=0,
    East,
    South,
    West,
}

pub struct Robot {
    x: i32,
    y: i32,
    d: Direction,
}

impl Robot {
    pub fn new(x: i32, y: i32, d: Direction) -> Self {
        Robot {x,y,d}
    }

    #[must_use]
    pub fn turn_right(self) -> Self {
        Robot::new(self.x, self.y, Direction::try_from((self.d as u8 +1) % 4).unwrap())
    }

    #[must_use]
    pub fn turn_left(self) -> Self {
        Robot::new(self.x, self.y, Direction::try_from((self.d as u8 +3) % 4).unwrap())
    }

    #[must_use]
    pub fn advance(self) -> Self {
        match self.d {
            Direction::East => Robot::new(self.x+1, self.y, self.d),
            Direction::South => Robot::new(self.x, self.y-1, self.d),
            Direction::West => Robot::new(self.x-1, self.y, self.d),
            Direction::North => Robot::new(self.x, self.y+1, self.d),
        }
    }

    #[must_use]
    pub fn instructions(self, instructions: &str) -> Self {
        instructions.chars().fold(self, |acc: Robot, x| {
            match x {
                'A' => acc.advance(),
                'L' => acc.turn_left(),
                'R' => acc.turn_right(),
                _ => acc,
            }
        })
    }

    pub fn position(&self) -> (i32, i32) {
        (self.x, self.y)
    }

    pub fn direction(&self) -> &Direction {
        &self.d
    }
}
