use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Tree<T: Debug + Ord> {
    value: T,
    children: Vec<Tree<T>>,
}

impl<T: Debug + Ord> Tree<T> {
    pub fn new(label: T) -> Self {
        Tree::<T> {
            value: label,
            children: vec![],
        }
    }

    /// Builder-method for constructing a tree with children
    pub fn with_child(self, child: Self) -> Self {
        let mut newt = self;
        newt.children.push(child);
        newt.children.sort();
        newt
    }

    fn path_to(&self, to: &T) -> Option<Vec<usize>> {
        if self.value == *to {
            return Some(vec![]);
        }

        for (i, c) in self.children.iter().enumerate() {
            let path = c.path_to(to);
            if let Some(mut path) = path {
                path.push(i);
                return Some(path);
            }
        }

        None
    }

    fn sort_children(&mut self) {
        self.children.sort();
        for c in self.children.iter_mut() {
            c.sort_children();
        }
    }

    pub fn pov_from(&mut self, from: &T) -> bool {
        let chain = self.path_to(from);

        if let Some(mut chain) = chain {
            chain.reverse();

            for x in chain {
                let mut new_root = self.children.remove(x);
                std::mem::swap(self, &mut new_root);
                self.children.push(new_root);
            }

            self.sort_children();
            true
        } else {
            false
        }
    }

    pub fn path_between<'a>(&'a mut self, from: &'a T, to: &'a T) -> Option<Vec<&'a T>> {
        if self.pov_from(from) {
            let path = self.path_to(to);

            let mut current = self;
            let mut res = Vec::<&'a T>::new();
            if let Some(mut path) = path {
                res.push(from);
                path.reverse();
                for p in path {
                    current = &mut current.children[p];
                    res.push(&current.value);
                }
                return Some(res);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn something_works() {
        // your test code here
        let tree = Tree::new("grandparent")
            .with_child(
                Tree::new("parent")
                    .with_child(
                        Tree::new("x")
                            .with_child(Tree::new("kid-0"))
                            .with_child(Tree::new("kid-1")),
                    )
                    .with_child(Tree::new("sibling-0"))
                    .with_child(Tree::new("sibling-1")),
            )
            .with_child(
                Tree::new("uncle")
                    .with_child(Tree::new("cousin-0"))
                    .with_child(Tree::new("cousin-1")),
            );
        let mut x = tree.path_to(&"kid-1").unwrap();
        x.reverse();
        println!("x={:?}", x);
    }
}
