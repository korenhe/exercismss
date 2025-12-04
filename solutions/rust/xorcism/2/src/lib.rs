use std::{borrow::Borrow, ops::BitXor};

/// A munger which XORs a key with some data
#[derive(Clone)]
pub struct Xorcism<'a> {
    key: &'a [u8],
    cursor: usize,
}

impl<'a> Xorcism<'a> {
    /// Create a new Xorcism munger from a key
    ///
    /// Should accept anything which has a cheap conversion to a byte slice.
    pub fn new<Key>(key: &'a Key) -> Xorcism<'a>
    where
        Key: Borrow<[u8]> + ?Sized,
    {
        let bytes: &[u8] = key.borrow();

        Xorcism {
            key: bytes,
            cursor: 0,
        }
    }

    /// XOR each byte of the input buffer with a byte from the key.
    ///
    /// Note that this is stateful: repeated calls are likely to produce different results,
    /// even with identical inputs.
    pub fn munge_in_place(&mut self, data: &mut [u8]) {
        data.iter_mut().for_each(|x| {
            *x = x.bitxor(self.key[self.cursor]);
            self.cursor = (self.cursor + 1) % self.key.len();
        });
    }

    /// XOR each byte of the data with a byte from the key.
    ///
    /// Note that this is stateful: repeated calls are likely to produce different results,
    /// even with identical inputs.
    ///
    /// Should accept anything which has a cheap conversion to a byte iterator.
    /// Shouldn't matter whether the byte iterator's values are owned or borrowed.
    pub fn munge<'b, Data>(&'b mut self, data: Data) -> impl Iterator<Item = u8> + 'b
    where
        Data: IntoIterator + 'b,
        Data::Item: Borrow<u8>,
    {
        data.into_iter().map(|y| {
            let res = (*y.borrow()).bitxor(self.key[self.cursor]);
            self.cursor = (self.cursor + 1) % self.key.len();
            res
        })
    }
}
