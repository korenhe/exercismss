use std::collections::HashMap;

pub type Value = i32;
pub type Result = std::result::Result<(), Error>;

#[derive(Debug, Clone)]
enum OperatorType {
    Add,
    Sub,
    Mul,
    Div,
    Dup,
    Drop,
    Swap,
    Over,
}

#[derive(Debug, Clone)]
enum Op {
    Operator(OperatorType),
    Symbol(String),
    Number(Value),
}

pub struct Forth {
    stack: Vec<Value>,
    symbols: HashMap<String, Vec<Op>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}

impl Forth {
    pub fn new() -> Forth {
        Forth {
            stack: vec![],
            symbols: HashMap::<String, Vec<Op>>::new(),
        }
    }

    pub fn stack(&self) -> &[Value] {
        &self.stack
    }

    fn str_to_op(
        &self,
        s: &str,
        hmap: &mut HashMap<String, Vec<Op>>,
        key: &str,
        dup: bool,
    ) -> Result {
        let mut tmp: Vec<Op> = vec![];
        let mut issymbol = false;

        // if dup definition
        if hmap.contains_key(s) {
            tmp = hmap.get(s).unwrap().to_vec();
            issymbol = true;
        }

        {
            let ops = &mut hmap.get_mut(key).unwrap();
            if issymbol {
                if s == key {
                    ops.clear();
                    tmp.iter().for_each(|x| {
                        ops.push(x.clone());
                    });
                } else {
                    ops.push(Op::Symbol(s.to_string()));
                }
                return Ok(());
            }

            if dup {
                ops.clear();
            }

            match s {
                "+" => ops.push(Op::Operator(OperatorType::Add)),
                "-" => ops.push(Op::Operator(OperatorType::Sub)),
                "*" => ops.push(Op::Operator(OperatorType::Mul)),
                "/" => ops.push(Op::Operator(OperatorType::Div)),
                "DUP" => ops.push(Op::Operator(OperatorType::Dup)),
                "DROP" => ops.push(Op::Operator(OperatorType::Drop)),
                "SWAP" => ops.push(Op::Operator(OperatorType::Swap)),
                "OVER" => ops.push(Op::Operator(OperatorType::Over)),
                _ => {
                    if let Ok(y) = s.parse() {
                        ops.push(Op::Number(y));
                    } else {
                        return Err(Error::UnknownWord);
                    }
                }
            }
        }
        Ok(())
    }

    pub fn eval(&mut self, input: &str) -> Result {
        let mut iter = input.split(' ').map(|x| x.to_uppercase());
        let mut symbols = HashMap::<String, Vec<Op>>::new();
        symbols.insert("global".to_string(), vec![]);
        self.symbols.iter().for_each(|(k, v)| {
            if k != "global" {
                symbols.insert(k.to_string(), v.to_vec());
            }
        });

        while let Some(xx) = iter.next() {
            match xx.as_str() {
                ":" => {
                    // get symbol name
                    let sym = iter.next().unwrap();
                    if let Ok(_x) = sym.parse::<Value>() {
                        return Err(Error::InvalidWord);
                    }

                    let mut dup = symbols.contains_key(&sym);
                    symbols.entry(sym.clone()).or_default();

                    // replace all sym in all symbols
                    if dup {
                        let old = symbols.get(&sym).unwrap().clone();
                        symbols.iter_mut().for_each(|(_k, v)| {
                            let mut i = 0;
                            while i < v.len() {
                                if let Op::Symbol(ref s) = v[i] {
                                    if *s == sym {
                                        v.splice(i..i + 1, old.clone());
                                        i += old.len(); // skip inserted elements
                                    } else {
                                        i += 1;
                                    }
                                } else {
                                    i += 1;
                                }
                            }
                        });
                    }

                    for xx in iter.by_ref() {
                        if xx != ";" {
                            let _ = self.str_to_op(&xx, &mut symbols, &sym, dup);
                            dup = false;
                        } else {
                            break;
                        }
                    }
                }
                _ => self.str_to_op(&xx, &mut symbols, "global", false)?,
            }
        }

        self.symbols = symbols.clone();
        self.eval_inner(symbols.get("global").unwrap().to_vec())
    }

    fn eval_inner(&mut self, ops: Vec<Op>) -> Result {
        // evaluate
        for op in ops.iter() {
            match op {
                Op::Operator(x) => match x {
                    OperatorType::Add => match (self.stack.pop(), self.stack.pop()) {
                        (Some(a), Some(b)) => {
                            self.stack.push(b + a);
                        }
                        _ => return Err(Error::StackUnderflow),
                    },
                    OperatorType::Sub => match (self.stack.pop(), self.stack.pop()) {
                        (Some(a), Some(b)) => {
                            self.stack.push(b - a);
                        }
                        _ => return Err(Error::StackUnderflow),
                    },
                    OperatorType::Mul => match (self.stack.pop(), self.stack.pop()) {
                        (Some(a), Some(b)) => {
                            self.stack.push(b * a);
                        }
                        _ => return Err(Error::StackUnderflow),
                    },
                    OperatorType::Div => match (self.stack.pop(), self.stack.pop()) {
                        (Some(a), Some(b)) => {
                            if a == 0 {
                                return Err(Error::DivisionByZero);
                            } else {
                                self.stack.push(b / a);
                            }
                        }
                        _ => return Err(Error::StackUnderflow),
                    },
                    OperatorType::Drop => {
                        if self.stack.pop().is_none() {
                            return Err(Error::StackUnderflow);
                        }
                    }
                    OperatorType::Dup => {
                        if !self.stack.is_empty() {
                            let a = *self.stack.last().expect("xx");
                            self.stack.push(a);
                        } else {
                            return Err(Error::StackUnderflow);
                        }
                    }
                    OperatorType::Swap => match (self.stack.pop(), self.stack.pop()) {
                        (Some(a), Some(b)) => {
                            self.stack.push(a);
                            self.stack.push(b);
                        }
                        _ => return Err(Error::StackUnderflow),
                    },
                    OperatorType::Over => {
                        if self.stack.len() > 1 {
                            let a = self.stack.get(self.stack.len() - 2).unwrap();
                            self.stack.push(*a);
                        } else {
                            return Err(Error::StackUnderflow);
                        }
                    }
                },
                Op::Number(num) => {
                    self.stack.push(*num);
                }
                Op::Symbol(sym) => self.eval_inner(self.symbols.get(sym).unwrap().to_vec())?,
            };
        }

        Ok(())
    }
}
