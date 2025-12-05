use std::{cell::RefCell, rc::Rc};

// this module adds some functionality based on the required implementations
// here like: `LinkedList::pop_back` or `Clone for LinkedList<T>`
// You are free to use anything in it, but it's mainly for the test framework.
mod pre_implemented;

#[derive(Debug)]
struct Node<T: std::fmt::Debug> {
    prev: Option<Rc<RefCell<Node<T>>>>,
    next: Option<Rc<RefCell<Node<T>>>>,
    value: Option<T>,
}

pub struct LinkedList<T: std::fmt::Debug> {
    head: Option<Rc<RefCell<Node<T>>>>,
    tail: Option<Rc<RefCell<Node<T>>>>,
}

//(std::marker::PhantomData<&'a mut T>);
pub struct Cursor<'a, T: std::fmt::Debug> {
    node: Option<Rc<RefCell<Node<T>>>>,
    list: &'a mut LinkedList<T>,
}

pub struct Iter<'a, T: std::fmt::Debug> {
    next: Option<Rc<RefCell<Node<T>>>>,
    _marker: std::marker::PhantomData<&'a T>,
}

impl<T: std::fmt::Debug> LinkedList<T> {
    pub fn new() -> Self {
        LinkedList {
            head: None,
            tail: None,
        }
    }

    // You may be wondering why it's necessary to have is_empty()
    // when it can easily be determined from len().
    // It's good custom to have both because len() can be expensive for some types,
    // whereas is_empty() is almost always cheap.
    // (Also ask yourself whether len() is expensive for LinkedList)
    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn len(&self) -> usize {
        if self.is_empty() {
            return 0;
        }

        let mut count = 0;
        let mut cur_opt = self.head.clone();
        while let Some(cur) = cur_opt {
            count += 1;
            cur_opt = cur.borrow().next.clone();
        }
        count
    }

    /// Return a cursor positioned on the front element
    pub fn cursor_front(&mut self) -> Cursor<T> {
        Cursor {
            node: self.head.clone(),
            list: self,
        }
    }

    /// Return a cursor positioned on the back element
    pub fn cursor_back(&mut self) -> Cursor<T> {
        Cursor {
            node: self.tail.clone(),
            list: self,
        }
    }

    /// Return an iterator that moves from front to back
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            next: self.head.clone(),
            _marker: std::marker::PhantomData,
        }
    }
}

// the cursor is expected to act as if it is at the position of an element
// and it also has to work with and be able to insert into an empty list.
impl<T: std::fmt::Debug> Cursor<'_, T> {
    /// Take a mutable reference to the current element
    pub fn peek_mut(&mut self) -> Option<&mut T> {
        let node_rc = self.node.as_ref()?;
        let mut_ref: &mut Node<T> = unsafe {
            // SAFETY: We promise no other &mut exists while cursor exists
            &mut *node_rc.as_ptr()
        };
        mut_ref.value.as_mut()
    }

    /// Move cursor forward (towards tail)
    pub fn next(&mut self) -> Option<&mut T> {
        let node_rc = self.node.as_ref()?;
        let next_rc = node_rc.borrow().next.clone()?;
        self.node = Some(next_rc.clone());

        let mut_ref: &mut Node<T> = unsafe { &mut *next_rc.as_ptr() };
        mut_ref.value.as_mut()
    }

    /// Move cursor backward (towards head)
    pub fn prev(&mut self) -> Option<&mut T> {
        let node_rc = self.node.as_ref()?;
        let prev_rc = node_rc.borrow().prev.clone()?;
        self.node = Some(prev_rc.clone());

        let mut_ref: &mut Node<T> = unsafe { &mut *prev_rc.as_ptr() };
        mut_ref.value.as_mut()
    }

    /// Remove and return the element at the current cursor, moving the cursor forward if possible
    pub fn take(&mut self) -> Option<T> {
        let node_rc = self.node.take()?;
        let prev = node_rc.borrow().prev.clone();
        let next = node_rc.borrow().next.clone();

        // Relink neighbors
        if let Some(prev_rc) = &prev {
            prev_rc.borrow_mut().next = next.clone();
        } else {
            self.list.head = next.clone();
        }

        if let Some(next_rc) = &next {
            next_rc.borrow_mut().prev = prev.clone();
        } else {
            self.list.tail = prev.clone();
        }

        self.node = next.or(prev);
        node_rc.borrow_mut().value.take()
    }

    pub fn insert_after(&mut self, element: T) {
        let new_node = Rc::new(RefCell::new(Node {
            value: Some(element),
            prev: None,
            next: None,
        }));

        if let Some(current_rc) = self.node.as_ref() {
            let mut current = current_rc.borrow_mut();
            let next = current.next.take();

            current.next = Some(new_node.clone());
            new_node.borrow_mut().prev = Some(current_rc.clone());

            if let Some(next_rc) = next {
                next_rc.borrow_mut().prev = Some(new_node.clone());
                new_node.borrow_mut().next = Some(next_rc);
            } else {
                // Was tail
                self.list.tail = Some(new_node.clone());
            }
        } else {
            // Empty list: insert as head & tail
            self.list.head = Some(new_node.clone());
            self.list.tail = Some(new_node.clone());
            self.node = Some(new_node);
        }
    }

    pub fn insert_before(&mut self, element: T) {
        let new_node = Rc::new(RefCell::new(Node {
            value: Some(element),
            prev: None,
            next: None,
        }));

        if let Some(current_rc) = self.node.as_ref() {
            let mut current = current_rc.borrow_mut();
            let prev = current.prev.take();

            current.prev = Some(new_node.clone());
            new_node.borrow_mut().next = Some(current_rc.clone());

            if let Some(prev_rc) = prev {
                prev_rc.borrow_mut().next = Some(new_node.clone());
                new_node.borrow_mut().prev = Some(prev_rc);
            } else {
                // Was head
                self.list.head = Some(new_node.clone());
            }
        } else {
            // Empty list: insert as head & tail
            self.list.head = Some(new_node.clone());
            self.list.tail = Some(new_node.clone());
            self.node = Some(new_node);
        }
    }
}

impl<'a, T: std::fmt::Debug> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        let node_rc = self.next.clone()?;
        self.next = node_rc.borrow().next.clone();
        // SAFETY: we only return &T; Rc guarantees the node lives as long as the list
        let ptr: *const Node<T> = &*node_rc.borrow();
        unsafe { (*ptr).value.as_ref() }
    }
}

impl<T: std::fmt::Debug> Drop for LinkedList<T> {
    fn drop(&mut self) {
        // Start from the head
        let mut cur_opt = self.head.take();
        while let Some(cur_rc) = cur_opt {
            // Take ownership of the current node by replacing its next with None
            let mut cur = cur_rc.borrow_mut();
            cur_opt = cur.next.take(); // move to the next node
            cur.prev = None; // break back link
            // now cur_rc goes out of scope and is dropped if no other Rc exists
        }
        // head and tail are already taken and None now
    }
}
