use std::collections::{HashMap, HashSet};

#[derive (Debug, Hash, PartialEq, Eq, Copy, Clone)]
enum Color {
    Spade,
    Club,
    Heart,
    Diamond,
}

#[derive (Debug)]
struct Card {
    color: Color,
    number: u8,
}

#[derive (Debug)]
struct Poker {
    cards: Vec<Card>,
    index: usize,
}

impl Card {
    pub fn new(name: &str) -> Self {
        let color = match name.chars().last() {
            Some('D') => Color::Diamond,
            Some('C') => Color::Club,
            Some('H') => Color::Heart,
            Some('S') => Color::Spade,
            _ => panic!("x")
        };
        let number_str = name.chars().take(name.len() - 1).collect::<String>();
        let number = match number_str.as_str() {
            "A" => 1,
            "J" => 11,
            "Q" => 12,
            "K" => 13,
            _ => number_str.parse().unwrap_or_else(|_| panic!("{} should be digit", number_str)),
        };

        Card { color, number}
    }
}

impl Poker {
    pub fn new (hand: &str, index: usize) -> Self {
        Poker {
            cards: hand.split(' ').map(Card::new).collect::<Vec<Card>>(),
            index
        }
    }

    fn is_continuous (&self) ->bool {
        let mut numbers = Vec::<u8>::new();
        self.cards.iter().for_each(|card|{
            numbers.push(card.number);
        });
        numbers.sort();
        if numbers[4] - numbers[0] == 4 {
            return true;
        } else if numbers[0] == 1 && numbers[4] == 13 {
            numbers.iter_mut().for_each(|x| {
                if *x< 6 {
                    *x+=13
                }
            });
            numbers.sort();
            return numbers[4] - numbers[0] == 4 && numbers[4] == 14;
        }
        false
    }

    fn updateace(res: &mut [u8]) {
        res.iter_mut().for_each(|x| if *x == 1 {
            *x = 14;
        });
    }
    pub fn value (&self) -> (u8, Vec<u8>) {
        let mut colors = HashSet::<Color>::new();
        let mut numbers = HashMap::<u8, usize>::new();
        self.cards.iter().for_each(|card| {
            colors.insert(card.color);
            *numbers.entry(card.number).or_insert(0) += 1;
        });

        let mut countsmap = numbers.iter().map(|(k,v)| (*v, *k)).collect::<Vec<(usize, u8)>>();
        countsmap.sort();

        let mut res: Vec<u8> = countsmap.iter().map(|(_x, y)| *y).collect::<Vec<u8>>();
        res.reverse();

        if colors.len() == 1 && self.is_continuous() {
            (1, res)
        } else if numbers.len() == 2 && countsmap[0].0 == 1 {
            Self::updateace(&mut res);
            (3, res)
        } else if numbers.len() == 2 && countsmap[0].0 == 2 {
            Self::updateace(&mut res);
            (4, res)
        } else if colors.len() == 1 {
            Self::updateace(&mut res);
            (5, res)
        } else if numbers.len() == 5 && self.is_continuous() {
            (6, res)
        } else if numbers.len() == 3 && countsmap[2].0 == 3{
            Self::updateace(&mut res);
            (7, res)
        } else if numbers.len() == 3 && countsmap[1].0 == 2 && countsmap[2].0 == 2 {
            Self::updateace(&mut res);
            (8, res)
        } else if numbers.len() == 4 && countsmap[3].0 == 2 {
            Self::updateace(&mut res);
            (9, res)
        } else {
            Self::updateace(&mut res);
            (10, res)
        }
    }
}


/// Given a list of poker hands, return a list of those hands which win.
///
/// Note the type signature: this function should return _the same_ reference to
/// the winning hand(s) as were passed in, not reconstructed strings which happen to be equal.
pub fn winning_hands<'a>(hands: &[&'a str]) -> Vec<&'a str> {
    let mut pokers = Vec::<Poker>::new();
    hands.iter().enumerate().for_each(|(i, x)| pokers.push(Poker::new(x, i)));
    let mut result = HashMap::<u8, Vec<(Vec<u8>, usize)>>::new();
    let mut best: u8 = 0;
    pokers.iter().for_each(|poker|  {
        let (level,xvalues) = poker.value();
        let mut values = Vec::<u8>::new();
        values.push(10-level);
        values.append(&mut xvalues.clone());

        best = best.max(10-level);
        result.entry(10-level).or_default().push((values, poker.index));
    });

    let candidates = result.get(&best).unwrap();

    let mut bestv = candidates[0].0.clone();
    candidates.iter().for_each(|(x, _y)| {
        if *x > bestv {
            bestv = x.clone();
        }
    });

    candidates.iter().filter(|(x, _y)| {
        *x == bestv
    }).map(|(_x, y)| hands[*y]).collect()
}
