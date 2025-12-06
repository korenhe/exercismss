use std::{cell::RefCell, marker::PhantomData, rc::Rc};

// this module adds some functionality based on the required implementations
// here like: `LinkedList::pop_back` or `Clone for LinkedList<T>`
// You are free to use anything in it, but it's mainly for the test framework.
mod pre_implemented;

#[derive(Debug)]
struct Node<T> {
    prev: Option<Rc<RefCell<Node<T>>>>,
    next: Option<Rc<RefCell<Node<T>>>>,
    value: Option<T>,
}

pub struct LinkedList<T> {
    head: Option<Rc<RefCell<Node<T>>>>,
    tail: Option<Rc<RefCell<Node<T>>>>,
}

//(std::marker::PhantomData<&'a mut T>);
pub struct Cursor<'a, T> {
    node: Option<Rc<RefCell<Node<T>>>>,
    list: &'a mut LinkedList<T>,
}

pub struct Iter<'a, T> {
    next: Option<Rc<RefCell<Node<T>>>>,
    _marker: std::marker::PhantomData<&'a T>,
}

impl<T> LinkedList<T> {
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

        // Reason R1: cursor is a solid reference to each node, not as_ref, but borrow, since the cursor should be valid across loop
        let mut cursor = self.head.clone();
        let mut count = 0;
        while let Some(node) = cursor {
            count += 1;
            cursor = node.borrow().next.clone();
        }
        count
    }

    /// Return a cursor positioned on the front element
    pub fn cursor_front(&mut self) -> Cursor<T> {
        Cursor {
            // same as R1
            node: self.head.clone(),
            list: self,
        }
    }

    /// Return a cursor positioned on the back element
    pub fn cursor_back(&mut self) -> Cursor<T> {
        Cursor {
            // same as R1
            node: self.tail.clone(),
            list: self,
        }
    }

    /// Return an iterator that moves from front to back
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            next: self.head.clone(),
            _marker: PhantomData,
        }
    }
}

// the cursor is expected to act as if it is at the position of an element
// and it also has to work with and be able to insert into an empty list.
impl<T> Cursor<'_, T> {
    /// Take a mutable reference to the current element
    pub fn peek_mut(&mut self) -> Option<&mut T> {
        let rcnode = self.node.as_ref()?;

        // res is with type Node<T>, and the function need to return Option<&mut
        // T>, so i need to return .as_mut() of it, if res is a local variable,
        // maybe i should use a reference to rcnode which is a solid and long
        // term item which can safely be returned out.
        let ptr: *mut Node<T> = &mut *rcnode.borrow_mut();
        let res = unsafe { &mut *ptr };
        res.value.as_mut()
    }

    /// Move cursor forward (towards tail)
    pub fn next(&mut self) -> Option<&mut T> {
        let rcnode = self.node.as_ref()?;

        let next_opt = rcnode.borrow().next.clone();
        if let Some(ref _next) = next_opt {
            self.node = next_opt;
            self.peek_mut()
        } else {
            None
        }
    }

    /// Move cursor backward (towards head)
    pub fn prev(&mut self) -> Option<&mut T> {
        let rcnode = self.node.as_ref()?;

        let prev_opt = rcnode.borrow().prev.clone();
        if let Some(ref _prev) = prev_opt {
            self.node = prev_opt;
            self.peek_mut()
        } else {
            None
        }
    }

    /// Remove and return the element at the current cursor, moving the cursor forward if possible
    pub fn take(&mut self) -> Option<T> {
        let rcnode = self.node.as_ref()?;
        let next_opt = rcnode.borrow().next.clone();
        let prev_opt = rcnode.borrow().prev.clone();

        if let Some(ref next) = next_opt {
            next.borrow_mut().prev = prev_opt.clone();
        } else {
            self.list.tail = prev_opt.clone();
        }

        if let Some(ref prev) = prev_opt {
            prev.borrow_mut().next = next_opt.clone();
        } else {
            self.list.head = next_opt.clone();
        }

        let cloned = self.node.clone()?;
        let ptr: *mut Node<T> = &mut *cloned.borrow_mut();
        let res = unsafe { &mut *ptr };
        let res = res.value.take();

        self.node = next_opt.or(prev_opt);

        res
    }

    pub fn insert_after(&mut self, element: T) {
        let rcnode = self.node.as_ref();
        if rcnode.is_none() {
            let new_rcnode = Rc::new(RefCell::new(Node {
                prev: None,
                next: None,
                value: Some(element),
            }));

            self.node = Some(new_rcnode.clone());
            self.list.tail = Some(new_rcnode.clone());
            self.list.head = Some(new_rcnode.clone());
            return;
        }

        let rcnode = rcnode.unwrap();
        let mut next_opt = rcnode.borrow_mut().next.clone();

        let new_rcnode = Rc::new(RefCell::new(Node {
            prev: Some(rcnode.clone()),
            next: next_opt.clone(),
            value: Some(element),
        }));

        rcnode.borrow_mut().next = Some(new_rcnode.clone());

        if let Some(next) = &mut next_opt {
            next.borrow_mut().prev = Some(new_rcnode.clone());
        } else {
            self.list.tail = Some(new_rcnode);
        }
    }

    pub fn insert_before(&mut self, element: T) {
        let rcnode = self.node.as_ref();
        if rcnode.is_none() {
            let new_rcnode = Rc::new(RefCell::new(Node {
                prev: None,
                next: None,
                value: Some(element),
            }));

            self.node = Some(new_rcnode.clone());
            self.list.tail = Some(new_rcnode.clone());
            self.list.head = Some(new_rcnode.clone());
            return;
        }

        let rcnode = rcnode.unwrap();
        let mut prev_opt = rcnode.borrow_mut().prev.clone();

        let new_rcnode = Rc::new(RefCell::new(Node {
            prev: prev_opt.clone(),
            next: Some(rcnode.clone()),
            value: Some(element),
        }));

        rcnode.borrow_mut().prev = Some(new_rcnode.clone());

        if let Some(prev) = &mut prev_opt {
            prev.borrow_mut().next = Some(new_rcnode.clone());
        } else {
            self.list.head = Some(new_rcnode);
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        let rcnode = self.next.clone()?;
        let next_opt = rcnode.borrow().next.clone();
        self.next = next_opt;

        let ptr: *const Node<T> = &*rcnode.borrow();
        let res = unsafe { &(*ptr) };

        res.value.as_ref()
    }
}

impl<T> Drop for LinkedList<T> {
    fn drop(&mut self) {
        let mut cursor = self.cursor_front();
        while cursor.take().is_some() {}
    }
}
