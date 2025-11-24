use num_enum::TryFromPrimitive;
use std::convert::TryFrom;

pub struct Allergies {
    score: u32
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Copy, Clone, TryFromPrimitive)]
pub enum Allergen {
    Eggs,
    Peanuts,
    Shellfish,
    Strawberries,
    Tomatoes,
    Chocolate,
    Pollen,
    Cats,
}

impl Allergies {
    pub fn new(score: u32) -> Self {
        Allergies {score}
    }

    pub fn is_allergic_to(&self, allergen: &Allergen) -> bool {
        self.score & (1 << (*allergen as u8))  != 0
    }

    pub fn allergies(&self) -> Vec<Allergen> {
        (Allergen::Eggs as u8..=Allergen::Cats as u8).filter(|x| {
            self.is_allergic_to(&Allergen::try_from(*x).unwrap())
        }).map(|x| Allergen::try_from(x).unwrap()).collect::<Vec<Allergen>>()
    }
}
