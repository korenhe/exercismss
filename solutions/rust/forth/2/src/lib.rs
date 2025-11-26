use std::collections::HashMap;

pub type Value = i32;
pub type Result = std::result::Result<(), Error>;
pub type ForthResult<T> = std::result::Result<T, Error>;

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

    fn expand_ops(ops: &[Op], sym: &str, body: &[Op]) -> Vec<Op> {
        ops.iter()
            .flat_map(|op| match op {
                Op::Symbol(s) if s == sym => body.to_vec(),
                _ => vec![op.clone()],
            })
            .collect()
    }

    fn str_to_op(
        &self,
        s: &str,
        hmap: &mut HashMap<String, Vec<Op>>,
        key: &str,
    ) -> ForthResult<Vec<Op>> {
        let mut tmp: Vec<Op> = vec![];
        let mut issymbol = false;
        let mut ops = Vec::<Op>::new();

        // if dup definition
        if hmap.contains_key(s) {
            tmp = hmap.get(s).unwrap().to_vec();
            issymbol = true;
        }

        if issymbol {
            ops.push(Op::Symbol(s.to_string()));
            if s == key {
                ops = Self::expand_ops(&ops, s, &tmp);
            }
            return Ok(ops);
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
        Ok(ops)
    }

    pub fn eval(&mut self, input: &str) -> Result {
        let mut iter = input.split(' ').map(|x| x.to_uppercase());
        let mut symbols = HashMap::<String, Vec<Op>>::new();

        // Move the old symbol table out of `self`
        let old_symbols = std::mem::take(&mut self.symbols);
        old_symbols.into_iter().for_each(|(k, v)| {
            if k != "global" {
                symbols.insert(k, v);
            }
        });
        symbols.insert("global".to_string(), vec![]);

        while let Some(xx) = iter.next() {
            match xx.as_str() {
                ":" => {
                    // get symbol name
                    let sym = iter.next().ok_or(Error::InvalidWord)?;
                    if let Ok(_x) = sym.parse::<Value>() {
                        return Err(Error::InvalidWord);
                    }

                    // replace all sym in all symbols
                    if symbols.contains_key(&sym) {
                        if let Some(old) = symbols.get(&sym).cloned() {
                            for ops in symbols.values_mut() {
                                *ops = Self::expand_ops(ops, &sym, &old);
                            }
                        }
                    }

                    symbols.entry(sym.clone()).or_default();
                    let mut symbolops = Vec::<Op>::new();
                    for xx in iter.by_ref() {
                        if xx != ";" {
                            symbolops.append(&mut self.str_to_op(&xx, &mut symbols, &sym)?);
                        } else {
                            break;
                        }
                    }
                    symbols.insert(sym, symbolops);
                }
                _ => {
                    let mut ops = self.str_to_op(&xx, &mut symbols, "global")?;
                    symbols
                        .entry("global".to_string())
                        .or_default()
                        .append(&mut ops);
                }
            }
        }

        let globalops = symbols.get("global").unwrap().to_vec();
        self.symbols = std::mem::take(&mut symbols);
        self.eval_inner(globalops)
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
