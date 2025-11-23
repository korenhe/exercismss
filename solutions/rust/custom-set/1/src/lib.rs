#[derive(Debug)]
pub struct CustomSet<T> {
    data: Vec<T>
}

impl<T> CustomSet<T>
where T: PartialEq + Clone
{
    pub fn new(input: &[T]) -> Self {
        let mut s = CustomSet {data: Vec::<T>::new()};
        input.iter().for_each(|x|{
            if !s.data.contains(x) {
                s.data.push(x.clone())
            }
        });
        s
    }

    pub fn contains(&self, element: &T) -> bool {
        self.data.contains(element)
    }

    pub fn add(&mut self, element: T) {
        if !self.data.contains(&element) {
            self.data.push(element)
        }
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        self.data.iter().all(|x| other.contains(x))
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn is_disjoint(&self, other: &Self) -> bool {
        !other.data.iter().any(|x| self.contains(x))
    }

    #[must_use]
    pub fn intersection(&self, other: &Self) -> Self {
        CustomSet::new(&other.data.iter().filter(|x| self.contains(x)).cloned().collect::<Vec<T>>())
    }

    #[must_use]
    pub fn difference(&self, other: &Self) -> Self {
        CustomSet::new(&self.data.iter().filter(|x| !other.contains(x)).cloned().collect::<Vec<T>>())
    }

    #[must_use]
    pub fn union(&self, other: &Self) -> Self {
        CustomSet::new(&other.data.iter().cloned().chain(self.data.clone()).collect::<Vec<T>>())
    }
}

impl<T: Clone + Ord> PartialEq for CustomSet<T> {
    fn eq(&self, other: &Self) -> bool {
        let mut a = self.data.clone();
        let mut b = other.data.clone();
        a.sort();
        b.sort();
        a == b
    }
}
