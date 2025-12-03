/// `InputCellId` is a unique identifier for an input cell.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InputCellId(usize);
/// `ComputeCellId` is a unique identifier for a compute cell.
/// Values of type `InputCellId` and `ComputeCellId` should not be mutually assignable,
/// demonstrated by the following tests:
///
/// ```compile_fail
/// let mut r = react::Reactor::new();
/// let input: react::ComputeCellId = r.create_input(111);
/// ```
///
/// ```compile_fail
/// let mut r = react::Reactor::new();
/// let input = r.create_input(111);
/// let compute: react::InputCellId = r.create_compute(&[react::CellId::Input(input)], |_| 222).unwrap();
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ComputeCellId(usize);
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CallbackId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CellId {
    Input(InputCellId),
    Compute(ComputeCellId),
}

#[derive(Debug, PartialEq, Eq)]
pub enum RemoveCallbackError {
    NonexistentCell,
    NonexistentCallback,
}

type ComputeFn<'a, T> = Box<dyn Fn(&[T]) -> T + 'a>;
pub struct ComputeCell<'a, T> {
    dependency_ids: Vec<CellId>,
    compute_func: ComputeFn<'a, T>,
    value: Option<T>,
    callback_ids: Vec<CallbackId>,
}

type CallbackFn<'a, T> = Box<dyn FnMut(T) + 'a>;
pub struct Reactor<'a, T> {
    inputs: Vec<T>,
    computes: Vec<ComputeCell<'a, T>>,
    callbacks: Vec<CallbackFn<'a, T>>,
}

// You are guaranteed that Reactor will only be tested against types that are Copy + PartialEq.
impl<'a, T: Copy + PartialEq> Reactor<'a, T>
where
    T: std::fmt::Debug,
{
    pub fn new() -> Self {
        Reactor {
            inputs: vec![],
            computes: vec![],
            callbacks: vec![],
        }
    }

    // Creates an input cell with the specified initial value, returning its ID.
    pub fn create_input(&mut self, initial: T) -> InputCellId {
        let id = self.inputs.len();
        self.inputs.push(initial);
        InputCellId(id)
    }

    // Creates a compute cell with the specified dependencies and compute function.
    // The compute function is expected to take in its arguments in the same order as specified in
    // `dependencies`.
    // You do not need to reject compute functions that expect more arguments than there are
    // dependencies (how would you check for this, anyway?).
    //
    // If any dependency doesn't exist, returns an Err with that nonexistent dependency.
    // (If multiple dependencies do not exist, exactly which one is returned is not defined and
    // will not be tested)
    //
    // Notice that there is no way to *remove* a cell.
    // This means that you may assume, without checking, that if the dependencies exist at creation
    // time they will continue to exist as long as the Reactor exists.
    pub fn create_compute<F>(
        &mut self,
        dependencies: &[CellId],
        compute_func: F,
    ) -> Result<ComputeCellId, CellId>
    where
        F: Fn(&[T]) -> T + 'a,
    {
        // validate
        for d in dependencies {
            match d {
                CellId::Compute(xd) => {
                    if xd.0 >= self.computes.len() {
                        return Err(*d);
                    }
                }
                CellId::Input(xd) => {
                    if xd.0 >= self.inputs.len() {
                        return Err(*d);
                    }
                }
            }
        }

        let compute_cell = ComputeCell {
            dependency_ids: dependencies.to_vec(),
            compute_func: Box::new(compute_func),
            value: None,
            callback_ids: vec![],
        };
        let id = self.computes.len();
        self.computes.push(compute_cell);
        self.do_compute(ComputeCellId(id));
        Ok(ComputeCellId(id))
    }

    // Retrieves the current value of the cell, or None if the cell does not exist.
    //
    // You may wonder whether it is possible to implement `get(&self, id: CellId) -> Option<&Cell>`
    // and have a `value(&self)` method on `Cell`.
    //
    // It turns out this introduces a significant amount of extra complexity to this exercise.
    // We chose not to cover this here, since this exercise is probably enough work as-is.
    pub fn value(&self, id: CellId) -> Option<T> {
        match id {
            CellId::Compute(id) => {
                // get dependencies
                let args = self.computes[id.0]
                    .dependency_ids
                    .iter()
                    .map(|x| self.value(*x))
                    .collect::<Option<Vec<_>>>();
                Some(self.computes[id.0].compute_func.as_ref()(&args.unwrap()))
            }
            CellId::Input(id) => Some(self.inputs[id.0]),
        }
    }

    pub fn set_compute(&mut self, id: ComputeCellId, new_value: T) -> bool {
        if id.0 < self.computes.len() {
            println!(
                "set compute old={:?}, new={:?}",
                self.computes[id.0].value, new_value
            );
            if self.computes[id.0].value != Some(new_value) {
                self.computes[id.0].value = Some(new_value);

                // execute callbacks
                for cid in &self.computes[id.0].callback_ids {
                    let cc = &mut self.callbacks[cid.0];
                    cc(new_value);
                }
            }

            // each computecell depend this cell should be updated, and each callback should be invoked by the compute cell's value
            for i in 0..self.computes.len() {
                let deps = &self.computes[i].dependency_ids;
                if deps.contains(&CellId::Compute(id)) {
                    let args = deps
                        .iter()
                        .map(|d| self.value(*d))
                        .collect::<Option<Vec<_>>>();

                    let val = (self.computes[i].compute_func)(&args.unwrap());
                    self.set_compute(ComputeCellId(i), val);
                }
            }
            true
        } else {
            false
        }
    }

    fn do_compute(&mut self, id: ComputeCellId) {
        let deps = &self.computes[id.0].dependency_ids;
        let args = deps
            .iter()
            .map(|d| self.value(*d))
            .collect::<Option<Vec<_>>>();

        let val = (self.computes[id.0].compute_func)(&args.unwrap());
        self.set_compute(ComputeCellId(id.0), val);
    }

    // Sets the value of the specified input cell.
    //
    // Returns false if the cell does not exist.
    //
    // Similarly, you may wonder about `get_mut(&mut self, id: CellId) -> Option<&mut Cell>`, with
    // a `set_value(&mut self, new_value: T)` method on `Cell`.
    //
    // As before, that turned out to add too much extra complexity.
    pub fn set_value(&mut self, id: InputCellId, new_value: T) -> bool {
        if id.0 < self.inputs.len() {
            self.inputs[id.0] = new_value;

            // each computecell depend this cell should be updated, and each callback should be invoked by the compute cell's value
            for i in 0..self.computes.len() {
                let deps = &self.computes[i].dependency_ids;
                if deps.contains(&CellId::Input(id)) {
                    self.do_compute(ComputeCellId(i));
                }
            }
            true
        } else {
            false
        }
    }

    // Adds a callback to the specified compute cell.
    //
    // Returns the ID of the just-added callback, or None if the cell doesn't exist.
    //
    // Callbacks on input cells will not be tested.
    //
    // The semantics of callbacks (as will be tested):
    // For a single set_value call, each compute cell's callbacks should each be called:
    // * Zero times if the compute cell's value did not change as a result of the set_value call.
    // * Exactly once if the compute cell's value changed as a result of the set_value call.
    //   The value passed to the callback should be the final value of the compute cell after the
    //   set_value call.
    pub fn add_callback<F: FnMut(T) + 'a>(
        &mut self,
        id: ComputeCellId,
        callback: F,
    ) -> Option<CallbackId> {
        if id.0 >= self.computes.len() {
            return None;
        }

        let cbid = self.callbacks.len();
        self.callbacks.push(Box::new(callback));
        self.computes[id.0].callback_ids.push(CallbackId(cbid));
        Some(CallbackId(cbid))
    }

    // Removes the specified callback, using an ID returned from add_callback.
    //
    // Returns an Err if either the cell or callback does not exist.
    //
    // A removed callback should no longer be called.
    pub fn remove_callback(
        &mut self,
        cell: ComputeCellId,
        callback: CallbackId,
    ) -> Result<(), RemoveCallbackError> {
        if cell.0 >= self.computes.len() {
            return Err(RemoveCallbackError::NonexistentCell);
        }

        let index = self.computes[cell.0]
            .callback_ids
            .iter()
            .position(|x| *x == callback);
        if let Some(index) = index {
            self.computes[cell.0].callback_ids.remove(index);
            Ok(())
        } else {
            Err(RemoveCallbackError::NonexistentCallback)
        }
    }
}
