pub struct CircularBuffer<T> {
    data: Vec<T>,
    rpos: usize,
    wpos: usize,
    filled: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    EmptyBuffer,
    FullBuffer,
}

impl<T: Clone + Default> CircularBuffer<T> {
    pub fn new(capacity: usize) -> Self {
        let mut data = Vec::<T>::new();
        data.resize_with(capacity, Default::default);
        CircularBuffer::<T> {
            data,
            rpos: 0,
            wpos: 0,
            filled: 0,
        }
    }

    fn nextpos(&self, pos: usize) -> usize{
        (pos + 1) % self.data.len()
    }

    fn is_empty(&self) -> bool {
        self.filled == 0
    }

    fn is_full(&self) -> bool {
        self.filled == self.data.len()
    }

    pub fn write(&mut self, element: T) -> Result<(), Error> {
        if self.is_full() {
            return Err(Error::FullBuffer);
        }
        self.data[self.wpos] = element;
        self.wpos = self.nextpos(self.wpos);
        self.filled += 1;
        Ok(())
    }

    pub fn read(&mut self) -> Result<T, Error> {
        if self.is_empty() {
            return Err(Error::EmptyBuffer);
        }
        let el: T = self.data[self.rpos].clone();
        self.rpos = self.nextpos(self.rpos);
        self.filled -= 1;
        Ok(el)
    }

    pub fn clear(&mut self) {
        self.rpos = 0;
        self.wpos = 0;
        self.filled = 0;
        for slot in &mut self.data {
            *slot = Default::default();
        }
    }

    pub fn overwrite(&mut self, element: T) {
        self.data[self.wpos] = element;
        self.wpos = self.nextpos(self.wpos);
        if self.filled == self.data.len() {
            self.rpos = self.nextpos(self.rpos);
        } else {
            self.filled += 1;
        }
    }
}
