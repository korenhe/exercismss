// the PhantomData instances in this file are just to stop compiler complaints
// about missing generics; feel free to remove them

use std::{fmt::Display, ops::Rem};

type MatchFn<T> = fn(T) -> bool;

/// A Matcher is a single rule of fizzbuzz: given a function on T, should
/// a word be substituted in? If yes, which word?
pub struct Matcher<'a, T> {
    matcher: MatchFn<T>,
    sub: &'a str,
}

impl<'a, T> Matcher<'a, T> {
    pub fn new(matcher: MatchFn<T>, sub: &'a str) -> Matcher<'a, T> {
        Matcher { matcher, sub }
    }
}

/// A Fizzy is a set of matchers, which may be applied to an iterator.
///
/// Strictly speaking, it's usually more idiomatic to use `iter.map()` than to
/// consume an iterator with an `apply` method. Given a Fizzy instance, it's
/// pretty straightforward to construct a closure which applies it to all
/// elements of the iterator. However, we're using the `apply` pattern
/// here because it's a simpler interface for students to implement.
///
/// Also, it's a good excuse to try out using impl trait.
pub struct Fizzy<'a, T> {
    matchers: Vec<Matcher<'a, T>>,
}

impl<'a, T: 'a> Fizzy<'a, T> {
    pub fn new() -> Self {
        Fizzy { matchers: vec![] }
    }

    // feel free to change the signature to `mut self` if you like
    #[must_use]
    pub fn add_matcher(self, matcher: Matcher<'a, T>) -> Self {
        let mut f: Fizzy<T> = self;
        f.matchers.push(matcher);
        f
    }

    /// map this fizzy onto every element of an iterator, returning a new iterator
    pub fn apply<I>(self, iter: I) -> impl Iterator<Item = String>
    where
        I: Iterator<Item = T>,
        T: Display + Copy,
    {
        // todo!() doesn't actually work, here; () is not an Iterator
        // that said, this is probably not the actual implementation you desire
        iter.map(move |x| {
            let mut res = Vec::<String>::new();
            self.matchers.iter().for_each(|m| {
                if (m.matcher)(x) {
                    res.push(m.sub.to_string());
                }
            });
            if res.is_empty() {
                res.push(x.to_string());
            }
            res.concat()
        })
    }
}

/// convenience function: return a Fizzy which applies the standard fizz-buzz rules
pub fn fizz_buzz<'a, T>() -> Fizzy<'a, T>
where
    T: Rem<Output = T> + PartialEq  + From<u8> + 'a,
{
    let fizzy = Fizzy::<T>::new();
    let fizzy = fizzy
        .add_matcher(Matcher {
            matcher: |x| x % T::from(3) == T::from(0),
            sub: "fizz",
        })
        .add_matcher(Matcher {
            matcher: |x| x % T::from(5) == T::from(0),
            sub: "buzz",
        });
    fizzy
}
