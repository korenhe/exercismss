// The code below is a stub. Just enough to satisfy the compiler.
// In order to pass the tests you can add-to or change any of this code.

#[derive(Debug)]
pub struct Duration {
    seconds: u64,
}

impl From<u64> for Duration {
    fn from(s: u64) -> Self {
        Duration {seconds : s}
    }
}

pub trait Planet {
    fn years_during(d: &Duration) -> f64 {
        todo!("convert a duration ({d:?}) to the number of years on this planet for that duration");
    }
}

pub struct Mercury;
pub struct Venus;
pub struct Earth;
pub struct Mars;
pub struct Jupiter;
pub struct Saturn;
pub struct Uranus;
pub struct Neptune;

macro_rules! otherplanet {
    ($name: ident, $arg: expr) => {
        impl Planet for $name {
            fn years_during(d: &Duration) -> f64 {
                Earth::years_during(d) / $arg
            }
        }
    };
}


impl Planet for Earth {
    fn years_during(d: &Duration) -> f64 {
        d.seconds as f64 / ((3600 * 24) as f64 * 365.24)
    }
}

otherplanet!(Mercury, 0.2408467);
otherplanet!(Venus, 0.61519726);
otherplanet!(Mars, 1.8808158);
otherplanet!(Jupiter, 11.862615);
otherplanet!(Saturn, 29.447498);
otherplanet!(Uranus, 84.016846);
otherplanet!(Neptune, 164.79132);
