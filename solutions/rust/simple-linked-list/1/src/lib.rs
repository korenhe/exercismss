struct Node<T> {
    data: T,
    next: Option<Box<Node<T>>>,
}

impl<T> Node<T> {
    pub fn new(element: T, next: Option<Box<Node<T>>>) -> Self {
        Node {
            data: element,
            next,
        }
    }
}

pub struct SimpleLinkedList<T> {
    head: Option<Box<Node<T>>>,
}

impl<T> SimpleLinkedList<T> {
    pub fn new() -> Self {
        SimpleLinkedList {
            head: None
        }
    }

    // You may be wondering why it's necessary to have is_empty()
    // when it can easily be determined from len().
    // It's good custom to have both because len() can be expensive for some types,
    // whereas is_empty() is almost always cheap.
    // (Also ask yourself whether len() is expensive for SimpleLinkedList)
    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn len(&self) -> usize {
        let mut cursor = self.head.as_ref();
        let mut count = 0;
        while let Some(node) = cursor {
            cursor = node.next.as_ref();
            count += 1;
        }
        count
    }

    pub fn push(&mut self, element: T) {
        let new_node = Box::new(Node::new(element, self.head.take()));
        self.head = Some(new_node);
    }

    pub fn pop(&mut self) -> Option<T> {
        let x = self.head.take();
        assert!(self.head.is_none());
        if let Some(y) = x {
            self.head = y.next;
            Some(y.data)
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<&T> {
        let x = self.head.as_ref();
        if let Some(y) = x {
            Some(&y.data)
        } else {
            None
        }
    }

    #[must_use]
    pub fn rev(&mut self) -> SimpleLinkedList<T> {
        let mut res = SimpleLinkedList::<T>::new();
        while let Some(top) = self.pop() {
            res.push(top);
        }
        res
    }
}

impl<T> FromIterator<T> for SimpleLinkedList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut y: SimpleLinkedList<T> = SimpleLinkedList::new();
        iter.into_iter().for_each(|x| y.push(x));
        y
    }
}

// In general, it would be preferable to implement IntoIterator for SimpleLinkedList<T>
// instead of implementing an explicit conversion to a vector. This is because, together,
// FromIterator and IntoIterator enable conversion between arbitrary collections.
//
// The reason this exercise's API includes an explicit conversion to Vec<T> instead
// of IntoIterator is that implementing that interface is fairly complicated, and
// demands more of the student than we expect at this point in the track.
//
// Please note that the "front" of the linked list should correspond to the "back"
// of the vector as far as the tests are concerned.

impl<T> From<SimpleLinkedList<T>> for Vec<T> {
    fn from(mut linked_list: SimpleLinkedList<T>) -> Vec<T> {
        let mut res = Vec::<T>::new();
        let mut rev = linked_list.rev();
        while let Some(top) = rev.pop() {
            res.push(top);
        }
        res
    }
}
